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
package org.conqat.engine.persistence.store.hist;

import org.conqat.engine.persistence.index.schema.EStorageOption;
import org.conqat.engine.persistence.index.schema.SchemaEntry;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.ByteArrayComparator;
import org.conqat.engine.persistence.store.branched.BranchCommitInfo;
import org.conqat.engine.persistence.store.branched.BranchCommitInsertingStore;
import org.conqat.engine.persistence.store.branched.BranchCommitReadingStore;
import org.conqat.engine.persistence.store.branched.BranchedStoreUtils;
import org.conqat.engine.persistence.store.mem.InMemoryStore;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.io.ByteArrayUtils;

/**
 * This class describes the options used to access a historized store.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51781 $
 * @ConQAT.Rating YELLOW Hash: 91C5F72F0B60227DC96017890EC43BA5
 */
public class HistoryAccessOption {

	/** The access mode. */
	private final EHistoryAccessMode access;

	/** The timestamp (interpretation depends on {@link #access}). */
	private final long timestamp;

	/** Hidden constructor. Use one of the factory methods instead, */
	private HistoryAccessOption(EHistoryAccessMode access, long timestamp) {
		CCSMPre.isTrue(timestamp > 0, "Timestamp must be positive.");
		this.access = access;
		this.timestamp = timestamp;
	}

	/** Read-only access to the head revision. */
	public static HistoryAccessOption readHead() {
		return new HistoryAccessOption(EHistoryAccessMode.READ_HEAD, 1);
	}

	/** Read from specified timestamp. */
	public static HistoryAccessOption readTimestamp(long timestamp) {
		return new HistoryAccessOption(EHistoryAccessMode.READ_TIMESTAMP,
				timestamp);
	}

	/** Write at new timestamp (reading from head). */
	public static HistoryAccessOption readHeadWriteTimestamp(long timestamp) {
		return new HistoryAccessOption(EHistoryAccessMode.WRITE_TIMESTAMP,
				timestamp);
	}

	/** Factory method for applying the options by wrapping a store. */
	public IStore createStore(IStore store, SchemaEntry schemaEntry)
			throws StorageException {
		if (schemaEntry.usesOption(EStorageOption.BRANCHED)) {
			return createBranchingStore(store);
		}

		if (schemaEntry.usesOption(EStorageOption.HISTORIZED)) {
			return createHistorizingStore(store);
		}

		throw new AssertionError(
				"Unkown store flags for supporting historization: "
						+ schemaEntry);
	}

	/** Creates a historizing store based on the type of access. */
	private IStore createHistorizingStore(IStore store) throws AssertionError {
		switch (access) {
		case READ_HEAD:
			return new HeadReadOnlyHistorizingStore(store);
		case READ_TIMESTAMP:
			return new TimestampReadOnlyHistorizingStore(store, timestamp);
		case WRITE_TIMESTAMP:
			return new HeadInsertingHistorizingStore(store, timestamp);

		default:
			throw new AssertionError("Unkown access mode: " + access);
		}
	}

	/** Creates a branching store based on the type of access. */
	private IStore createBranchingStore(IStore store) throws StorageException {
		switch (access) {
		case READ_HEAD:
			BranchCommitInfo headCommit = getHeadCommit(store);
			if (headCommit == null) {
				return new InMemoryStore();
			}
			return new BranchCommitReadingStore(store,
					headCommit.getCommitName());
		case READ_TIMESTAMP:
			BranchCommitInfo bestCommit = getNewestCommitBeforeOrAt(timestamp,
					store);
			if (bestCommit == null) {
				return new InMemoryStore();
			}
			return new BranchCommitReadingStore(store,
					bestCommit.getCommitName());
		case WRITE_TIMESTAMP:
			return createWritableBranchingStore(store);

		default:
			throw new AssertionError("Unkown access mode: " + access);
		}
	}

	/** Creates a branching store that writes at timestamp {@link #timestamp}. */
	private IStore createWritableBranchingStore(IStore store)
			throws StorageException {
		// TODO (TK): Combine next two lines?
		BranchCommitInfo headCommit;
		headCommit = getHeadCommit(store);
		if (headCommit == null) {
			return new BranchCommitInsertingStore(store,
					ByteArrayUtils.longToByteArray(timestamp), null);
		}
		int timestampComparisonResult = ByteArrayComparator.INSTANCE.compare(
				headCommit.getCommitName(),
				ByteArrayUtils.longToByteArray(timestamp));
		if (timestampComparisonResult > 0) {
			throw new StorageException(
					"Trying to insert older data at timestamp "
							+ timestamp
							+ ", which is older than current head (timestamp "
							+ BranchedStoreUtils
									.timestampFromCommitKey(headCommit
											.getCommitName()) + ")");
		}
		if (timestampComparisonResult == 0) {
			return new BranchCommitInsertingStore(store,
					headCommit.getCommitName(),
					headCommit.getParentCommitName());
		}
		return new BranchCommitInsertingStore(store,
				ByteArrayUtils.longToByteArray(timestamp),
				headCommit.getCommitName());
	}

	/** Returns the head commit for the given store (or null if empty). */
	private static BranchCommitInfo getHeadCommit(IStore store)
			throws StorageException {
		return getNewestCommitBeforeOrAt(Long.MAX_VALUE, store);
	}

	/** Returns the newest commit at or before the given timestamp (or null). */
	private static BranchCommitInfo getNewestCommitBeforeOrAt(long timestamp,
			IStore store) throws StorageException {
		byte[] bestKey = null;
		long bestTimestamp = Long.MAX_VALUE;
		for (byte[] commitKey : BranchedStoreUtils.getCommitKeys(store)) {
			long commitTimestamp = BranchedStoreUtils
					.timestampFromCommitKey(commitKey);
			if (commitTimestamp <= timestamp
					&& (bestKey == null || commitTimestamp > bestTimestamp)) {
				bestKey = commitKey;
				bestTimestamp = commitTimestamp;
			}
		}

		if (bestKey == null) {
			return null;
		}
		return (BranchCommitInfo) StorageUtils.deserialize(store.get(bestKey));
	}

	/**
	 * Returns the timestamp of this history access option. This is 1 in case of
	 * reading head.
	 */
	public long getTimestamp() {
		return timestamp;
	}

	/** Returns whether this is configured to read head */
	public boolean isReadHead() {
		return access == EHistoryAccessMode.READ_HEAD;
	}

	/** The access flags. */
	protected static enum EHistoryAccessMode {

		/** Read from head. */
		READ_HEAD,

		/** Read from specific timestamp. */
		READ_TIMESTAMP,

		/** Read from head, write to timestamp. */
		WRITE_TIMESTAMP
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return access + "@" + timestamp;
	}
}
