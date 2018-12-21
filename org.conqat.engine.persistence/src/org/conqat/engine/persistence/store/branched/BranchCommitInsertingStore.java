/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.persistence.store.branched;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.util.ExceptionHandlingKeyValueCallbackBase;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.concurrent.ThreadUtils;
import org.conqat.lib.commons.digest.Digester;

/**
 * A branch supporting store that allows to insert data into a new commit.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51574 $
 * @ConQAT.Rating GREEN Hash: 3BCEB1B6DDCCBE3FEFC002D3DFD711BD
 */
public class BranchCommitInsertingStore extends BranchCommitReadingStore {

	/** The logger. */
	private static final Logger LOGGER = Logger
			.getLogger(BranchCommitInsertingStore.class);

	/**
	 * Global object used for synchronization of commit creation within one
	 * process. Cross-process synchronization is performed by explicit status
	 * checks in the {@link BranchCommitInfo}.
	 */
	private static final Object COMMIT_CREATION_LOCK = new Object();

	/**
	 * Milliseconds to wait between consecutive polls of the the creation status
	 * of a commit.
	 */
	private static final int CREATION_WAIT_POLLING_TIME_MILLIS = 20;

	/**
	 * We wait for up to 5 minutes. The situation that a creation could not be
	 * completed should be a very rare one, so having a higher number here
	 * should not be too bad.
	 */
	private static final int MAX_CREATION_WAIT_TIME_MILLIS = 5 * 60 * 1000;

	/** The name of the commit to be written. */
	private final byte[] commitName;

	/**
	 * Constructor.
	 *
	 * @param parentCommitName
	 *            the name of the parent commit. This may be null to indicate an
	 *            initial commit.
	 */
	public BranchCommitInsertingStore(IStore delegate, byte[] commitName,
			byte[] parentCommitName) throws StorageException {
		super(delegate);
		this.commitName = commitName;
		createCommitInfo(parentCommitName);
		commitInfos = loadReferencedCommits(commitName);
	}

	/**
	 * Creates the commit info for the given commit name (unless the commit info
	 * already exists). This method performs synchronization to ensure that no
	 * two stores attempt to create a commit entry at the same time.
	 */
	private void createCommitInfo(byte[] parentCommitName)
			throws StorageException {
		boolean mustWaitForCreation = false;
		boolean mustCreateCommit = false;

		synchronized (COMMIT_CREATION_LOCK) {
			BranchCommitInfo commit = readCommit(commitName);
			if (commit == null) {
				// store dummy commit to ensure no one else creates a commit at
				// the same time
				writeCommit(new BranchCommitInfo(commitName, parentCommitName,
						null, 0, ECommitStatus.CREATING));
				mustCreateCommit = true;
			} else {
				switch (commit.getStatus()) {
				case SEALED:
					throw new StorageException("Can not write to commit "
							+ formatCommitName(commitName)
							+ " as it is already sealed!");
				case CREATING:
					mustWaitForCreation = true;
					// fall through intended
				case WRITEABLE:
					checkForConsistentParent(parentCommitName, commit);
					break;
				default:
					CCSMAssert.fail("Unknown status: " + commit.getStatus());
				}
			}
		}

		// perform waiting and/or creation outside of the synchronized area to
		// ensure that this does not block other threads.
		if (mustWaitForCreation) {
			// This case only happens, if another thread (or process) is
			// currently creating a commit. Then it is ok, to wait a few
			// milliseconds.
			mustCreateCommit = !waitForCreation();
		}
		if (mustCreateCommit) {
			createCommit(parentCommitName);
		}
	}

	/**
	 * Checks whether the parent of an existing commit is consistent with the
	 * parent we wanted to have for the commit.
	 */
	private void checkForConsistentParent(byte[] parentCommitName,
			BranchCommitInfo commit) throws StorageException {
		if (!Arrays.equals(parentCommitName, commit.getParentCommitName())) {
			throw new StorageException(
					"Inconsistent parent for writing commit "
							+ formatCommitName(commitName)
							+ ". Tried to write "
							+ formatCommitName(parentCommitName)
							+ " but stored is "
							+ formatCommitName(commit.getParentCommitName()));
		}
	}

	/**
	 * Waits until the commit with given name has been created and has its
	 * status set to writeable. Returns true if a writeable commit was found.
	 */
	private boolean waitForCreation() throws StorageException {
		int maxIterations = MAX_CREATION_WAIT_TIME_MILLIS
				/ CREATION_WAIT_POLLING_TIME_MILLIS;
		for (int i = 0; i < maxIterations; ++i) {
			ThreadUtils.sleep(CREATION_WAIT_POLLING_TIME_MILLIS);

			BranchCommitInfo commit = readCommit(commitName);
			switch (commit.getStatus()) {
			case SEALED:
				throw new StorageException("Can not write to commit "
						+ formatCommitName(commitName)
						+ " as it is already sealed!");
			case CREATING:
				// continue waiting
				break;
			case WRITEABLE:
				// we are ready
				return true;
			default:
				CCSMAssert.fail("Unknown status: " + commit.getStatus());
			}
		}

		// in case that the previous loops ended, we still have an unfinished
		// commit. This means we either had another thread working on it and the
		// thread was aborted for some reason, or the thread just takes so long
		// to complete this operation (load, size of commit, storage timeouts).
		// In both cases, the best guess is to create the commit ourselves. In
		// most cases, this will only cause duplicate work, but in some cases
		// this can lead to data loss.
		LOGGER.warn("Had a timeout while waiting for the creation of commit "
				+ formatCommitName(commitName)
				+ ". This is usually no problem, "
				+ "but might lead to inconsistent data or data-loss in some rare scenarios.");
		return false;
	}

	/**
	 * Creates the commit with given name and parent. If required, partial data
	 * is copied to this commit (see the description in the package.html for
	 * details).
	 */
	private void createCommit(byte[] parentCommitName) throws StorageException {
		if (parentCommitName == null) {
			writeCommit(new BranchCommitInfo(commitName, null, null, 1,
					ECommitStatus.WRITEABLE));
			return;
		}

		BranchCommitInfo parentCommit = readCommit(parentCommitName);
		if (parentCommit == null) {
			throw new StorageException("Parent commit "
					+ formatCommitName(parentCommitName) + " was not found!");
		}

		// seal parent commit, so no more changes can be applied to it
		if (parentCommit.getStatus() != ECommitStatus.SEALED) {
			writeCommit(new BranchCommitInfo(parentCommit, ECommitStatus.SEALED));
		}

		List<byte[]> mergeCommitNames = new ArrayList<>();
		byte[] deltaPredecessorCommitName = fillMergeCommitNamesAndDeterminDeltaPredecessor(
				parentCommit, mergeCommitNames);

		copyMergeCommitValues(mergeCommitNames,
				deltaPredecessorCommitName == null);

		writeCommit(new BranchCommitInfo(commitName,
				parentCommit.getCommitName(), deltaPredecessorCommitName,
				parentCommit.getDepth() + 1, ECommitStatus.WRITEABLE));
	}

	/**
	 * Copies the values from the given merge commits to this commit.
	 *
	 * @param mergeCommitNames
	 *            the names of commits to copy from. The order in this list
	 *            follows the predecessor hierarchy, i.e. elements towards the
	 *            end of the list are (grand)parents of elements towards the
	 *            start of the list.
	 * @param persistTombStones
	 *            if this is true, tomb stones result in "real" deleted.
	 *            Otherwise, just the tombstones are copied. This can only be
	 *            used if the entire parent hierarchy is copied, as otherwise
	 *            deleted might not be stored correctly.
	 */
	private void copyMergeCommitValues(List<byte[]> mergeCommitNames,
			final boolean persistTombStones) throws StorageException {
		for (byte[] mergeCommitName : CollectionUtils.reverse(mergeCommitNames)) {
			final int offset = mergeCommitName.length
					+ COMMIT_KEY_SEPARATOR.length;

			ExceptionHandlingKeyValueCallbackBase callback = new ExceptionHandlingKeyValueCallbackBase() {
				@Override
				protected void callbackWithException(byte[] key, byte[] value)
						throws StorageException {
					byte[] originalKey = Arrays.copyOfRange(key, offset,
							key.length);
					if (persistTombStones && isTombStone(value)) {
						store.remove(commitEntryKey(originalKey));
					} else {
						store.put(commitEntryKey(originalKey), value);
					}
				}
			};

			store.scan(commitEntryKey(mergeCommitName, null), callback);
			callback.throwCaughtException();
		}
	}

	/**
	 * Fills the list of merge commits, i.e. commits whose data should be copied
	 * to this commit. Returns the delta predecessor to use. The calculation
	 * here might seem slightly magic. See the package.html for more details.
	 */
	private byte[] fillMergeCommitNamesAndDeterminDeltaPredecessor(
			BranchCommitInfo parentCommit, List<byte[]> mergeCommitNames)
			throws StorageException {
		int commitDepth = parentCommit.getDepth() + 1;
		byte[] deltaPredecessorCommitName = parentCommit.getCommitName();
		while ((commitDepth & 1) == 0) {
			commitDepth >>= 1;
			mergeCommitNames.add(deltaPredecessorCommitName);
			BranchCommitInfo deltaPredecessorCommit = readCommit(deltaPredecessorCommitName);
			deltaPredecessorCommitName = deltaPredecessorCommit
					.getDeltaPredecessorCommitName();
		}
		return deltaPredecessorCommitName;
	}

	/** Stores the given commit in the store. */
	private void writeCommit(BranchCommitInfo commit) throws StorageException {
		store.put(commitKey(commit.getCommitName()),
				StorageUtils.serialize(commit));
	}

	/** {@inheritDoc} */
	@Override
	public void put(byte[] key, byte[] value) throws StorageException {
		store.put(commitEntryKey(key), deduplicateValue(value));
	}

	/** {@inheritDoc} */
	@Override
	public void put(PairList<byte[], byte[]> keysValues)
			throws StorageException {
		PairList<byte[], byte[]> adjustedKeysValues = new PairList<>();
		for (int i = 0; i < keysValues.size(); ++i) {
			byte[] key = keysValues.getFirst(i);
			byte[] value = keysValues.getSecond(i);
			adjustedKeysValues
					.add(commitEntryKey(key), deduplicateValue(value));
		}
		store.put(adjustedKeysValues);
	}

	/**
	 * Deduplicates the value by creating a reference key, storing the value
	 * under the reference and returning the reference hash value. For short
	 * values where deduplication does not make sense, the value itself is
	 * returned.
	 */
	private byte[] deduplicateValue(byte[] value) throws StorageException {
		// no need to deduplicate values shorter than the actual hash
		if (value == null || value.length < DATA_REFERENCE_HASH_LENGTH) {
			return value;
		}

		byte[] referenceKey = Digester.createBinarySHA1Digest(value);
		store.put(dataKey(referenceKey), value);
		return referenceKey;
	}

	/** {@inheritDoc} */
	@Override
	public void remove(byte[] key) throws StorageException {
		store.put(commitEntryKey(key), TOMB_STONE_MARKER);
	}

	/** {@inheritDoc} */
	@Override
	public void remove(List<byte[]> keys) throws StorageException {
		PairList<byte[], byte[]> tombStones = new PairList<>();
		for (byte[] key : keys) {
			tombStones.add(commitEntryKey(key), TOMB_STONE_MARKER);
		}
		store.put(tombStones);
	}

	/** Returns the commit entry key for the currently written commit. */
	private byte[] commitEntryKey(byte[] key) {
		return commitEntryKey(commitName, key);
	}
}
