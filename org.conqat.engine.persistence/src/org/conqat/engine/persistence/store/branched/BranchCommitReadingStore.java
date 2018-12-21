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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.StoreBase;
import org.conqat.engine.persistence.store.util.ExceptionHandlingKeyValueCallbackBase;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.collections.ByteArrayWrapper;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.io.ByteArrayUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * A store that reads the view defined by a specific commit. This is read-only.
 *
 * For each commit, there is one key for storing a {@link BranchCommitInfo}
 * instance and for each data entry in this commit a key prefixed with the
 * commit name.
 *
 * The store supports data deduplication, by storing larger values not directly,
 * but instead a reference to the actual value. The actual value is stored in an
 * artificial data key. The references are generated using SHA-1.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51574 $
 * @ConQAT.Rating GREEN Hash: 9166BB7F2EB2CEEDD11719B7613A8E7D
 */
public class BranchCommitReadingStore extends StoreBase {

	/** Length of the hash values used as data references in bytes. */
	protected static final int DATA_REFERENCE_HASH_LENGTH = 20;

	/** A special value marker used to identify a deleted entry. */
	protected static final byte[] TOMB_STONE_MARKER = new byte[] { -1 };

	/**
	 * Separator between the commit name and the actual key name in the key of
	 * data values.
	 */
	protected static final byte[] COMMIT_KEY_SEPARATOR = StringUtils
			.stringToBytes("#-#");

	/** Prefix for the keys storing the {@link BranchCommitInfo} instance. */
	public static final byte[] COMMIT_KEY_PREFIX = StringUtils
			.stringToBytes("#-#COMMIT#-#");

	/** Prefix for keys storing deduplicated data values. */
	private static final byte[] DATA_KEY_PREFIX = StringUtils
			.stringToBytes("#-#DATA#-#");

	/** The underlying store. */
	protected final IStore store;

	/**
	 * The relevant commit infos for this store. The first entry in the list is
	 * the commit currently being read, the following commits are all its delta
	 * predecessors.
	 */
	protected List<BranchCommitInfo> commitInfos;

	/** Constructor. */
	protected BranchCommitReadingStore(IStore delegate) {
		this.store = delegate;
	}

	/** Constructor. */
	public BranchCommitReadingStore(IStore delegate, byte[] commitName)
			throws StorageException {
		this(delegate);
		commitInfos = loadReferencedCommits(commitName);
	}

	/**
	 * Loads the commit referenced by the given commit name and all its delta
	 * predecessors.
	 */
	protected List<BranchCommitInfo> loadReferencedCommits(byte[] commitName)
			throws StorageException {
		List<BranchCommitInfo> result = new ArrayList<>();
		while (commitName != null) {
			BranchCommitInfo commit = readCommit(commitName);
			if (commit == null) {
				throw new StorageException(
						"Inconsistent store: commit with name "
								+ formatCommitName(commitName)
								+ " could not be read!");
			}

			result.add(commit);
			commitName = commit.getDeltaPredecessorCommitName();
		}
		return result;
	}

	/** Reads the commit with given name from the store. */
	protected BranchCommitInfo readCommit(byte[] commitName)
			throws StorageException {
		return (BranchCommitInfo) StorageUtils.deserialize(store
				.get(commitKey(commitName)));
	}

	/** Returns the key for storing the commit with the given name. */
	protected byte[] commitKey(byte[] commitName) {
		return ByteArrayUtils.concat(COMMIT_KEY_PREFIX, commitName);
	}

	/** Formats a commit name as a readable name */
	protected String formatCommitName(byte[] commitName) {
		return StringUtils.bytesToString(commitName) + " ("
				+ StringUtils.encodeAsHex(commitName) + ")";
	}

	/** {@inheritDoc} */
	@Override
	public byte[] get(byte[] key) throws StorageException {
		for (BranchCommitInfo commit : commitInfos) {
			byte[] value = store
					.get(commitEntryKey(commit.getCommitName(), key));

			// in this case, the value was deleted
			if (isTombStone(value)) {
				return null;
			}

			if (value != null) {
				return resolveDataReference(value, store);
			}
		}

		// never written before
		return null;
	}

	/**
	 * Resolves a data reference and returns the referenced data. If the
	 * provided value can not be a reference hash, the value itself is returned.
	 * On input of null, null is returned as well.
	 */
	public static byte[] resolveDataReference(byte[] value, IStore store)
			throws StorageException {
		if (value == null || value.length != DATA_REFERENCE_HASH_LENGTH) {
			return value;
		}

		byte[] referencedValue = store.get(dataKey(value));
		if (referencedValue == null) {
			throw new StorageException(
					"Corrupt branching store, data for reference "
							+ StringUtils.encodeAsHex(value) + " not found!");
		}

		return referencedValue;
	}

	/** {@inheritDoc} */
	@Override
	public List<byte[]> get(List<byte[]> keys) throws StorageException {
		// for multiple get query in the branched case it is still faster to
		// retrieve individual keys instead of reading all keys for all
		// revisions
		List<byte[]> result = new ArrayList<byte[]>();
		for (byte[] key : keys) {
			result.add(get(key));
		}
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public void put(byte[] key, byte[] value) throws StorageException {
		throw new StorageException("This is a read-only store!");
	}

	/** {@inheritDoc} */
	@Override
	public void put(PairList<byte[], byte[]> keysValues)
			throws StorageException {
		throw new StorageException("This is a read-only store!");
	}

	/** {@inheritDoc} */
	@Override
	public void remove(byte[] key) throws StorageException {
		throw new StorageException("This is a read-only store!");
	}

	/** {@inheritDoc} */
	@Override
	public void remove(List<byte[]> keys) throws StorageException {
		throw new StorageException("This is a read-only store!");
	}

	/** {@inheritDoc} */
	@Override
	public void scan(byte[] beginKey, byte[] endKey, IKeyValueCallback callback)
			throws StorageException {
		scanKeysValues(beginKey, endKey, callback, true);
	}

	/** {@inheritDoc} */
	@Override
	public void scanKeys(byte[] beginKey, byte[] endKey,
			IKeyValueCallback callback) throws StorageException {
		scanKeysValues(beginKey, endKey, callback, false);
	}

	/** Implementation for the scan methods. */
	private void scanKeysValues(byte[] beginKey, byte[] endKey,
			final IKeyValueCallback callback, final boolean includeValue)
			throws StorageException {
		final Set<ByteArrayWrapper> processedKeys = new HashSet<>();
		for (BranchCommitInfo commitInfo : commitInfos) {
			final int offset = commitInfo.getCommitName().length
					+ COMMIT_KEY_SEPARATOR.length;
			ExceptionHandlingKeyValueCallbackBase innerCallback = new ExceptionHandlingKeyValueCallbackBase() {

				@Override
				protected void callbackWithException(byte[] key, byte[] value)
						throws StorageException {
					byte[] originalKey = Arrays.copyOfRange(key, offset,
							key.length);
					if (!processedKeys.add(new ByteArrayWrapper(originalKey))) {
						return;
					}

					if (isTombStone(value)) {
						return;
					}

					if (includeValue) {
						callback.callback(originalKey,
								resolveDataReference(value, store));
					} else {
						callback.callback(originalKey, null);
					}
				}
			};
			store.scan(determineExtendedBeginKey(beginKey, commitInfo),
					determineExtendedEndKey(endKey, commitInfo), innerCallback);
			innerCallback.throwCaughtException();
		}
	}

	/** Returns the extended begin key for a given begin key and commit. */
	private byte[] determineExtendedBeginKey(byte[] beginKey,
			BranchCommitInfo commitInfo) {
		byte[] extendedBeginKey;
		if (beginKey == null) {
			extendedBeginKey = commitEntryKey(commitInfo.getCommitName(), null);
		} else {
			extendedBeginKey = commitEntryKey(commitInfo.getCommitName(),
					beginKey);
		}
		return extendedBeginKey;
	}

	/** Returns the extended end key for a given end key and commit. */
	private byte[] determineExtendedEndKey(byte[] endKey,
			BranchCommitInfo commitInfo) {
		byte[] extendedEndKey;
		if (endKey == null) {
			extendedEndKey = generateEndKey(commitEntryKey(
					commitInfo.getCommitName(), null));
		} else {
			extendedEndKey = commitEntryKey(commitInfo.getCommitName(), endKey);
		}
		return extendedEndKey;
	}

	/**
	 * Returns whether the given value corresponds to the
	 * {@link #TOMB_STONE_MARKER}.
	 */
	public static boolean isTombStone(byte[] value) {
		return value != null && value.length == 1
				&& value[0] == TOMB_STONE_MARKER[0];
	}

	/**
	 * Builds the key for storing entries of the commit from a commit name and a
	 * base key. The base key may be null to construct a key for prefix
	 * scanning.
	 */
	public static byte[] commitEntryKey(byte[] commitBinaryName, byte[] key) {
		if (key == null) {
			return ByteArrayUtils
					.concat(commitBinaryName, COMMIT_KEY_SEPARATOR);
		}
		return ByteArrayUtils.concat(commitBinaryName, COMMIT_KEY_SEPARATOR,
				key);
	}

	/**
	 * Returns the name of the key used for storing data with the given
	 * reference hash.
	 */
	protected static byte[] dataKey(byte[] referenceHash) {
		return ByteArrayUtils.concat(DATA_KEY_PREFIX, referenceHash);
	}
}
