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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.conqat.engine.persistence.index.schema.EStorageOption;
import org.conqat.engine.persistence.index.schema.SchemaEntry;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.ByteArrayComparator;
import org.conqat.engine.persistence.store.branched.BranchCommitReadingStore;
import org.conqat.engine.persistence.store.branched.BranchedStoreUtils;
import org.conqat.engine.persistence.store.util.ExceptionHandlingKeyValueCallbackBase;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.io.ByteArrayUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Utility code for dealing with historized stores.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51782 $
 * @ConQAT.Rating GREEN Hash: 4886A7F27FC47F3439356019C97AD961
 */
public class HistorizationUtils {

	/**
	 * Returns the history for a key. This is a {@link PairList} from the
	 * timestamp to the value. For a deletion, the value is null. There is
	 * guarantee on ordering.
	 *
	 * @param start
	 *            the start timestamp (inclusive)
	 * @param end
	 *            the end timestamp (exclusive)
	 *
	 * @param rawStore
	 *            the raw store (without history layer).
	 */
	public static PairList<Long, byte[]> queryHistory(final byte[] key,
			long start, long end, IStore rawStore, SchemaEntry schemaEntry)
			throws StorageException {

		if (schemaEntry.usesOption(EStorageOption.BRANCHED)) {
			return queryHistoryForBranchedStore(key, start, end, rawStore);
		}
		if (schemaEntry.usesOption(EStorageOption.HISTORIZED)) {
			return queryHistoryForHistorizedStore(key, start, end, rawStore);
		}
		throw new AssertionError(
				"Unknown historization support in schema entry: " + schemaEntry);
	}

	/**
	 * Returns the history for a key in a historized store. See
	 * {@link #queryHistory(byte[], long, long, IStore, SchemaEntry)} for
	 * parameters and the expected result.
	 */
	private static PairList<Long, byte[]> queryHistoryForHistorizedStore(
			final byte[] key, long start, long end, IStore rawStore)
			throws StorageException {
		final PairList<Long, byte[]> result = new PairList<Long, byte[]>();

		final byte[] startKey = HistorizingStoreBase.revisionKey(key,
				ByteArrayUtils.longToByteArray(start));
		byte[] endKey = HistorizingStoreBase.revisionKey(key,
				ByteArrayUtils.longToByteArray(end));

		ExceptionHandlingKeyValueCallbackBase callback = new ExceptionHandlingKeyValueCallbackBase() {

			@Override
			protected void callbackWithException(byte[] key, byte[] value)
					throws StorageException {
				// filter any other keys in between
				if (key.length != startKey.length) {
					return;
				}

				if (HistorizingStoreBase.isDeletionValue(value)) {
					value = null;
				}

				byte[] revisionPart = Arrays.copyOfRange(key, key.length
						- ByteArrayUtils.LONG_BYTE_ARRAY_LENGTH, key.length);
				try {
					result.add(ByteArrayUtils.byteArrayToLong(revisionPart),
							value);
				} catch (IOException e) {
					throw new StorageException("Invalid key encountered: "
							+ StringUtils.encodeAsHex(key), e);
				}
			}
		};

		rawStore.scan(startKey, endKey, callback);
		callback.throwCaughtException();
		return result;
	}

	/**
	 * Returns the history for a key in a branched store. See
	 * {@link #queryHistory(byte[], long, long, IStore, SchemaEntry)} for
	 * parameters and the expected result.
	 */
	private static PairList<Long, byte[]> queryHistoryForBranchedStore(
			byte[] key, long start, long end, IStore rawStore)
			throws StorageException {

		List<byte[]> queryKeys = new ArrayList<>();
		List<Long> timestamps = new ArrayList<>();

		List<byte[]> commitKeys = StorageUtils.listKeysStartingWith(
				BranchCommitReadingStore.COMMIT_KEY_PREFIX, rawStore);

		// sort chronologically (longs are serialized in a way that allows
		// sorting by byte array as well); sortedness is required below
		Collections.sort(queryKeys, ByteArrayComparator.INSTANCE);

		for (byte[] commitKey : commitKeys) {
			long commitTimestamp = BranchedStoreUtils
					.timestampFromCommitKey(commitKey);
			if (commitTimestamp >= start && commitTimestamp < end) {
				timestamps.add(commitTimestamp);
				queryKeys.add(BranchCommitReadingStore.commitEntryKey(
						ByteArrayUtils.longToByteArray(commitTimestamp), key));
			}
		}

		return extractHistoryData(rawStore, queryKeys, timestamps);
	}

	/**
	 * Extracts history data from a branched store.
	 * 
	 * @param rawStore
	 *            the store to get the data from.
	 * @param queryKeys
	 *            the keys used to query the values for which history data
	 *            should be extracted. The keys should appear in chronological
	 *            order.
	 * @param timestamps
	 *            the timestamps for the queryKeys (same order)
	 */
	private static PairList<Long, byte[]> extractHistoryData(IStore rawStore,
			List<byte[]> queryKeys, List<Long> timestamps)
			throws StorageException {
		List<byte[]> values = rawStore.get(queryKeys);

		PairList<Long, byte[]> result = new PairList<Long, byte[]>();
		byte[] prevValue = null;
		for (int i = 0; i < queryKeys.size(); ++i) {
			byte[] value = values.get(i);
			if (value == null) {
				continue;
			}
			if (prevValue != null && Arrays.equals(value, prevValue)) {
				// this check is only valid if we list keys in chronological
				// order
				continue;
			}
			prevValue = value;
			if (BranchCommitReadingStore.isTombStone(value)) {
				result.add(timestamps.get(i), null);
			} else {
				result.add(timestamps.get(i), BranchCommitReadingStore
						.resolveDataReference(value, rawStore));
			}
		}

		return result;
	}

}
