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

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.io.ByteArrayUtils;

/**
 * Utility code for dealing with branching stores.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51782 $
 * @ConQAT.Rating GREEN Hash: 8FE4DF9E3EEFD8FB536414D6AEDBFC6D
 */
public class BranchedStoreUtils {

	/** Deletes all commits after the given timestamp. */
	public static void deleteCommitsAfter(IStore store, long timestamp)
			throws StorageException {
		for (byte[] commitKey : getCommitKeys(store)) {
			long commitTimestamp = timestampFromCommitKey(commitKey);
			if (commitTimestamp > timestamp) {
				deleteCommit(store, commitKey);
			}
		}
	}

	/** Deletes the given commit. */
	private static void deleteCommit(IStore store, byte[] commitKey)
			throws StorageException {
		store.remove(commitKey);
		store.removeByPrefix(BranchCommitReadingStore.commitEntryKey(
				extractCommitNameFromCommitKey(commitKey), null));
	}

	/** Lists all commit keys for a branching store. */
	public static List<byte[]> getCommitKeys(IStore store)
			throws StorageException {
		return StorageUtils.listKeysStartingWith(
				BranchCommitReadingStore.COMMIT_KEY_PREFIX, store);
	}

	/** Returns the timestamp for a given commit key. */
	public static long timestampFromCommitKey(byte[] commitKey)
			throws StorageException {
		try {
			return ByteArrayUtils
					.byteArrayToLong(extractCommitNameFromCommitKey(commitKey));
		} catch (IOException e) {
			throw new StorageException(e);
		}
	}

	/**
	 * Extracts the name of the commit from the commit's storage key. This
	 * assumes that commit names are always stored as long values (timestamp).
	 */
	private static byte[] extractCommitNameFromCommitKey(byte[] commitKey) {
		return Arrays.copyOfRange(commitKey, commitKey.length
				- ByteArrayUtils.LONG_BYTE_ARRAY_LENGTH, commitKey.length);
	}
}
