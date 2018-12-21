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
package org.conqat.engine.persistence.store.cassandra;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import me.prettyprint.cassandra.serializers.BytesArraySerializer;
import me.prettyprint.cassandra.serializers.StringSerializer;
import me.prettyprint.cassandra.service.template.ColumnFamilyResult;
import me.prettyprint.cassandra.service.template.ColumnFamilyTemplate;
import me.prettyprint.cassandra.service.template.ColumnFamilyUpdater;
import me.prettyprint.hector.api.Keyspace;
import me.prettyprint.hector.api.beans.HColumn;
import me.prettyprint.hector.api.beans.OrderedRows;
import me.prettyprint.hector.api.beans.Row;
import me.prettyprint.hector.api.exceptions.HTimedOutException;
import me.prettyprint.hector.api.exceptions.HectorException;
import me.prettyprint.hector.api.factory.HFactory;
import me.prettyprint.hector.api.mutation.Mutator;
import me.prettyprint.hector.api.query.MultigetSliceQuery;
import me.prettyprint.hector.api.query.RangeSlicesQuery;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.TimeoutException;
import org.conqat.engine.persistence.store.base.PartitionStoreBase;
import org.conqat.engine.persistence.store.util.KeyCollectingCallback;
import org.conqat.lib.commons.collections.ByteArrayWrapper;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.collections.PairList;

/**
 * Store implementation for the {@link CassandraStorageSystem}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51583 $
 * @ConQAT.Rating GREEN Hash: 178C53AF71480C78B18024F24B17908C
 */
public class CassandraStore extends PartitionStoreBase {

	/** Maximal number of rows returned per page when scanning for rows. */
	private static final int MAX_SCAN_ROWS = 100;

	/**
	 * Maximal number of rows to include in batch operations (get, put, remove).
	 */
	private static final int MAX_BATCH_ROWS = 1000;

	/** We try hard to put not more than 10MB in a single batch put. */
	private static final int MAX_BATCH_PUT_BYTES = 10 * 1024 * 1024;

	/**
	 * Maximal number of bytes allowed in a single put. The default settings
	 * allow 15MB per thrift request. We choose 14 MB here, so there should be
	 * enough buffer for protocol overhead.
	 */
	private static final int MAX_SINGLE_PUT_BYTES = 14 * 1024 * 1024;

	/** The keyspace used for storing data. */
	private final Keyspace keyspace;

	/** Template for the column family. */
	private final ColumnFamilyTemplate<byte[], String> template;

	/** Constructor. */
	public CassandraStore(String name, Keyspace keyspace,
			ColumnFamilyTemplate<byte[], String> template) {
		super(name);
		this.keyspace = keyspace;
		this.template = template;
	}

	/** {@inheritDoc} */
	@Override
	protected byte[] doGet(byte[] key) throws StorageException {
		try {
			ColumnFamilyResult<byte[], String> res = template.queryColumns(key);
			return res.getByteArray(CassandraStorageSystem.COLUMN_NAME);
		} catch (HectorException e) {
			handleHectorException(e);
			return null;
		}
	}

	/** Handles a {@link HectorException} . */
	private void handleHectorException(HectorException e)
			throws StorageException {
		if (e instanceof HTimedOutException) {
			throw new TimeoutException(e);
		} else if (e.getMessage().startsWith("All host pools marked down")) {
			throw new TimeoutException(e);
		}
		throw new StorageException(e);
	}

	/** {@inheritDoc} */
	@Override
	protected List<byte[]> doBatchGet(List<byte[]> keys)
			throws StorageException {
		try {
			List<byte[]> result = new ArrayList<byte[]>();
			for (int i = 0; i < keys.size(); i += MAX_BATCH_ROWS) {
				List<byte[]> subKeyList = keys.subList(i,
						Math.min(i + MAX_BATCH_ROWS, keys.size()));
				result.addAll(batchGetSubList(subKeyList));
			}
			return result;
		} catch (HectorException e) {
			handleHectorException(e);
			return null;
		}
	}

	/** Implements a batch get operation for a list of keys. */
	private List<byte[]> batchGetSubList(List<byte[]> keys) {
		MultigetSliceQuery<byte[], String, byte[]> multigetSliceQuery = HFactory
				.createMultigetSliceQuery(keyspace, BytesArraySerializer.get(),
						StringSerializer.get(), BytesArraySerializer.get());
		multigetSliceQuery
				.setColumnFamily(CassandraStorageSystem.COLUMN_FAMILY_NAME);
		multigetSliceQuery.setKeys(keys);
		multigetSliceQuery.setColumnNames(CassandraStorageSystem.COLUMN_NAME);

		Map<ByteArrayWrapper, byte[]> keyToValue = new HashMap<>();
		for (Row<byte[], String, byte[]> row : multigetSliceQuery.execute()
				.get()) {
			List<HColumn<String, byte[]>> columns = row.getColumnSlice()
					.getColumns();
			if (!columns.isEmpty()) {
				keyToValue.put(new ByteArrayWrapper(row.getKey()),
						columns.get(0).getValue());
			}
		}

		List<byte[]> resultList = new ArrayList<byte[]>();
		for (byte[] key : keys) {
			resultList.add(keyToValue.get(new ByteArrayWrapper(key)));
		}
		return resultList;
	}

	/** {@inheritDoc} */
	@Override
	protected void doPut(byte[] key, byte[] value) throws StorageException {
		checkPutSizeConstraint(key, value);

		try {
			ColumnFamilyUpdater<byte[], String> updater = template
					.createUpdater(key);
			updater.setByteArray(CassandraStorageSystem.COLUMN_NAME, value);
			template.update(updater);
		} catch (HectorException e) {
			handleHectorException(e);
		}
	}

	/** Checks the size for individual puts. */
	public static void checkPutSizeConstraint(byte[] key, byte[] value)
			throws StorageException {
		int size = key.length + value.length;
		if (size > MAX_SINGLE_PUT_BYTES) {
			throw new StorageException("Had a single put with key of size "
					+ key.length + " and value of size " + value.length
					+ " which exceeds the overall limit of "
					+ MAX_SINGLE_PUT_BYTES + " bytes!");
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doBatchPut(PairList<byte[], byte[]> keysValues)
			throws StorageException {
		try {
			Mutator<byte[]> mutator = template.createMutator();
			int byteCount = 0;
			for (int i = 0; i < keysValues.size(); ++i) {
				byte[] key = keysValues.getFirst(i);
				byte[] value = keysValues.getSecond(i);
				checkPutSizeConstraint(key, value);

				// we force execution if there is at least one entry and adding
				// this key/value pair would violate the batch put size
				boolean forceExecution = byteCount > 0
						&& (byteCount + key.length + value.length) > MAX_BATCH_PUT_BYTES;
				if ((i + 1) % MAX_BATCH_ROWS == 0 || forceExecution) {
					mutator.execute();
					byteCount = 0;
				}
				byteCount += key.length + value.length;

				HColumn<String, byte[]> column = HFactory.createColumn(
						CassandraStorageSystem.COLUMN_NAME, value);
				mutator.addInsertion(key,
						CassandraStorageSystem.COLUMN_FAMILY_NAME, column);

			}
			mutator.execute();
		} catch (HectorException e) {
			handleHectorException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doRemove(byte[] key) throws StorageException {
		try {
			template.deleteRow(key);
		} catch (HectorException e) {
			handleHectorException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doBatchRemove(List<byte[]> keys) throws StorageException {
		try {
			Mutator<byte[]> mutator = template.createMutator();
			int index = 0;
			for (byte[] key : keys) {
				mutator.addDeletion(key,
						CassandraStorageSystem.COLUMN_FAMILY_NAME);
				index += 1;
				if (index % MAX_BATCH_ROWS == 0) {
					mutator.execute();
				}
			}
			mutator.execute();
		} catch (HectorException e) {
			handleHectorException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void doRemoveByPrefix(byte[] prefix) throws StorageException {
		byte[] beginKey = prefix;
		byte[] endKey = generateEndKey(prefix);

		try {
			RangeSlicesQuery<byte[], String, byte[]> query = prepareRangeQuery(
					beginKey, endKey);
			byte[] lastKey = null;
			while (true) {
				if (lastKey != null) {
					query.setKeys(lastKey, endKey);
				}

				OrderedRows<byte[], String, byte[]> rows = query.execute()
						.get();

				List<byte[]> keys = new ArrayList<>();
				Pair<byte[], Integer> lastKeyAndCount = reportScanData(rows,
						new KeyCollectingCallback(keys), endKey, false);
				lastKey = lastKeyAndCount.getFirst();

				doBatchRemove(keys);

				if (rows.getCount() < MAX_SCAN_ROWS) {
					return;
				}
			}
		} catch (HectorException e) {
			handleHectorException(e);
		}

	}

	/** {@inheritDoc} */
	@Override
	protected void doScan(byte[] beginKey, byte[] endKey,
			IKeyValueCallback callback, boolean includeValue)
			throws StorageException {
		try {
			RangeSlicesQuery<byte[], String, byte[]> query = prepareRangeQuery(
					beginKey, endKey);
			byte[] lastKey = null;
			while (true) {
				if (lastKey != null) {
					query.setKeys(lastKey, endKey);
				}

				OrderedRows<byte[], String, byte[]> rows = query.execute()
						.get();

				Pair<byte[], Integer> lastKeyAndCount = reportScanData(rows,
						callback, endKey, includeValue);
				lastKey = lastKeyAndCount.getFirst();

				if (rows.getCount() < MAX_SCAN_ROWS) {
					return;
				}
			}
		} catch (HectorException e) {
			handleHectorException(e);
		}
	}

	/**
	 * Reports the data in the given row to the callback. Returns the last
	 * reported key (or null if no data was reported).
	 */
	private Pair<byte[], Integer> reportScanData(
			OrderedRows<byte[], String, byte[]> rows,
			IKeyValueCallback callback, byte[] endKey, boolean includeValue) {
		byte[] lastKey = null;
		int count = 0;
		for (Row<byte[], String, byte[]> row : rows) {
			byte[] key = row.getKey();
			lastKey = key;

			// cassandra's end key is inclusive, ours is not
			if (Arrays.equals(endKey, key)) {
				continue;
			}

			byte[] value = null;
			List<HColumn<String, byte[]>> columns = row.getColumnSlice()
					.getColumns();
			// this check is required to filter keys/values that have already
			// been deleted but are still reported
			if (columns.isEmpty()) {
				continue;
			}

			if (includeValue) {
				value = columns.get(0).getValue();
			}
			callback.callback(key, value);
			count += 1;
		}
		return new Pair<>(lastKey, count);
	}

	/** Creates and initializes a range query. */
	private RangeSlicesQuery<byte[], String, byte[]> prepareRangeQuery(
			byte[] beginKey, byte[] endKey) {
		RangeSlicesQuery<byte[], String, byte[]> query = HFactory
				.createRangeSlicesQuery(keyspace, BytesArraySerializer.get(),
						StringSerializer.get(), BytesArraySerializer.get());
		query.setColumnFamily(CassandraStorageSystem.COLUMN_FAMILY_NAME);
		query.setColumnNames(CassandraStorageSystem.COLUMN_NAME);
		query.setRowCount(MAX_SCAN_ROWS);
		query.setKeys(beginKey, endKey);
		return query;
	}
}
