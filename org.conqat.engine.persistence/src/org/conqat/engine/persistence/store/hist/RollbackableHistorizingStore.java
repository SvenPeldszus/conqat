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
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.conqat.engine.persistence.rollback.IRollbackableIndex;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.ByteArrayComparator;
import org.conqat.engine.persistence.store.util.ExceptionHandlingKeyValueCallbackBase;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.io.ByteArrayUtils;

/**
 * Historizing store that supports rollback.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47898 $
 * @ConQAT.Rating GREEN Hash: D6F3D2FF04A704606DB79601544B85EE
 */
public class RollbackableHistorizingStore extends HeadReadOnlyHistorizingStore
		implements IRollbackableIndex {

	/**
	 * Maximal number of keys to process at the same time during HEAD recovery.
	 * This limit is required to avoid out of memory for large values.
	 */
	private static final int HEAD_RECOVERY_BATCH_SIZE = 100;

	/**
	 * Maximal number of keys we store in the delete cache to not run out of
	 * memory.
	 */
	private static final int DELETE_CACHE_SIZE = 1000;

	/** Constructor. */
	public RollbackableHistorizingStore(IStore delegate) {
		super(delegate);
	}

	/** {@inheritDoc} */
	@Override
	public void performRollback(final long timestamp) throws StorageException {
		RollbackCallback callback = new RollbackCallback(timestamp);
		store.scanKeys(new byte[0], callback);
		callback.persistResults();
	}

	/** Extracts the timestamp part of a key. */
	private static long getKeyTimestamp(byte[] key) throws StorageException {
		try {
			return ByteArrayUtils.byteArrayToLong(Arrays.copyOfRange(key,
					key.length - ByteArrayUtils.LONG_BYTE_ARRAY_LENGTH,
					key.length));
		} catch (IOException e) {
			throw new StorageException(e);
		}
	}

	/** Callback used for performing a rollback. */
	private class RollbackCallback extends
			ExceptionHandlingKeyValueCallbackBase {

		/** The largest timestamp that may be preserved in the store. */
		private final long timestamp;

		/** Keys that should be deleted. */
		private final List<byte[]> toDeleteCache = new ArrayList<>();

		/** The set of all keys that have to be updated in the head view. */
		private final Set<byte[]> headChangeKeys = new TreeSet<>(
				ByteArrayComparator.INSTANCE);

		/** Constructor. */
		public RollbackCallback(long timestamp) {
			this.timestamp = timestamp;
		}

		/** {@inheritDoc} */
		@Override
		protected void callbackWithException(byte[] key, byte[] value)
				throws StorageException {
			if (ByteArrayUtils.isPrefix(HEAD_PREFIX, key)) {
				return;
			}

			long keyTimestamp = getKeyTimestamp(key);
			if (keyTimestamp <= timestamp) {
				// keep this key
				return;
			}

			toDeleteCache.add(key);
			checkDeleteChache(false);

			// only store original key in head change keys
			headChangeKeys.add(Arrays.copyOf(key, key.length - 1
					- ByteArrayUtils.LONG_BYTE_ARRAY_LENGTH));
		}

		/**
		 * Checks if the delete cache is full and deletes the keys if so. In
		 * case of force, the delete cache is applied in any case.
		 */
		private void checkDeleteChache(boolean force) throws StorageException {
			if (force || toDeleteCache.size() >= DELETE_CACHE_SIZE) {
				store.remove(toDeleteCache);
				toDeleteCache.clear();
			}
		}

		/** Persists the results of the rollback operation. */
		public void persistResults() throws StorageException {
			throwCaughtException();
			checkDeleteChache(true);

			List<byte[]> headChangeKeysList = new ArrayList<>(headChangeKeys);
			TimestampReadOnlyHistorizingStore timestampStore = new TimestampReadOnlyHistorizingStore(
					store, timestamp);
			for (int i = 0; i < headChangeKeysList.size(); i += HEAD_RECOVERY_BATCH_SIZE) {
				int end = Math.min(i + HEAD_RECOVERY_BATCH_SIZE,
						headChangeKeysList.size());
				refreshHead(headChangeKeysList.subList(i, end), timestampStore);
			}
		}

		/**
		 * Refreshes the head view for the given keys.
		 * 
		 * @param timestampStore
		 *            a view on the {@link #timestamp} of the store.
		 */
		private void refreshHead(List<byte[]> headKeys,
				TimestampReadOnlyHistorizingStore timestampStore)
				throws StorageException {
			List<byte[]> values = timestampStore.get(headKeys);

			PairList<byte[], byte[]> toUpdate = new PairList<>();
			List<byte[]> toDelete = new ArrayList<>();

			for (int i = 0; i < headKeys.size(); ++i) {
				if (values.get(i) == null) {
					toDelete.add(headKey(headKeys.get(i)));
				} else {
					toUpdate.add(headKey(headKeys.get(i)), values.get(i));
				}
			}

			if (!toDelete.isEmpty()) {
				store.remove(toDelete);
			}
			if (!toUpdate.isEmpty()) {
				store.put(toUpdate);
			}
		}
	}
}
