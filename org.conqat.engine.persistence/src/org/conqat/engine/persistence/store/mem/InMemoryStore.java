/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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
package org.conqat.engine.persistence.store.mem;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;

import org.conqat.engine.persistence.store.IKeyValueCallback;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.ByteArrayComparator;
import org.conqat.engine.persistence.store.base.StoreBase;
import org.conqat.engine.persistence.store.util.IKeyValueConstraint;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * Store implementation that keeps all data in main memory. Very simple
 * persistence is possible, but all data must fit into main memory.
 * 
 * This implementation also supports snapshots, which can simplify a reset
 * during testing.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51583 $
 * @ConQAT.Rating GREEN Hash: 44D2E822D238E52E355971A96B683654
 */
public class InMemoryStore extends StoreBase {

	/** Data store. */
	private NavigableMap<byte[], byte[]> data = new TreeMap<byte[], byte[]>(
			ByteArrayComparator.INSTANCE);

	/** A snapshot of the data. */
	private NavigableMap<byte[], byte[]> snapshot;

	/** Constraints to use for put operations. */
	private final List<IKeyValueConstraint> putConstraints;

	/** Constructor. */
	public InMemoryStore() {
		this.putConstraints = new ArrayList<>();
	}

	/** Constructor. */
	public InMemoryStore(List<IKeyValueConstraint> putConstraints) {
		this.putConstraints = putConstraints;
	}

	/** {@inheritDoc} */
	@Override
	public synchronized byte[] get(byte[] key) {
		byte[] value = data.get(key);
		if (value == null) {
			return null;
		}
		return value.clone();
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void put(byte[] key, byte[] value)
			throws StorageException {
		for (IKeyValueConstraint constraint : putConstraints) {
			constraint.check(key, value);
		}

		data.put(key.clone(), value.clone());
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void remove(byte[] key) {
		data.remove(key);
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void scan(byte[] beginKey, byte[] endKey,
			IKeyValueCallback callback) {
		doScan(beginKey, endKey, callback, true, -1);
	}


	/** {@inheritDoc} */
	@Override
	public synchronized void scanKeys(byte[] beginKey, byte[] endKey,
			IKeyValueCallback callback) {
		doScan(beginKey, endKey, callback, true, -1);
	}


	/**
	 * Performs a scan on the data. This does not use iterators to prevent
	 * concurrent modification exceptions. Instead we manage an index key.
	 * 
	 * @param maxResults
	 *            maximal number of results to return. Use negative value for
	 *            unlimited.
	 */
	private void doScan(byte[] beginKey, byte[] endKey,
			IKeyValueCallback callback, boolean includeValue, int maxResults) {
		if (data.isEmpty()) {
			return;
		}

		Entry<byte[], byte[]> currentEntry;
		if (beginKey == null) {
			currentEntry = data.firstEntry();
		} else {
			currentEntry = data.ceilingEntry(beginKey);
		}

		while (currentEntry != null
				&& (endKey == null || ByteArrayComparator.INSTANCE.compare(
						currentEntry.getKey(), endKey) < 0)) {
			byte[] value = null;
			if (includeValue) {
				value = currentEntry.getValue().clone();
			}
			callback.callback(currentEntry.getKey().clone(), value);

			currentEntry = data.higherEntry(currentEntry.getKey());

			if (maxResults > 0) {
				maxResults -= 1;
			}
			if (maxResults == 0) {
				return;
			}
		}
	}

	/**
	 * Serializes this store to the given file. The contents of this file will
	 * be overwritten.
	 */
	public synchronized void dumpToFile(File file) throws StorageException {
		ObjectOutputStream out = null;
		try {
			out = new ObjectOutputStream(new BufferedOutputStream(
					new FileOutputStream(file)));
			out.writeObject(data);
		} catch (IOException e) {
			throw new StorageException("Could not persist store: "
					+ e.getMessage(), e);
		} finally {
			FileSystemUtils.close(out);
		}
	}

	/** Loads all entries from a map which was serialized to the given file. */
	@SuppressWarnings("unchecked")
	public synchronized void loadFromFile(File file) throws StorageException {
		ObjectInputStream in = null;
		try {
			in = new ObjectInputStream(new BufferedInputStream(
					new FileInputStream(file)));
			Map<byte[], byte[]> m = (Map<byte[], byte[]>) in.readObject();
			data.putAll(m);
		} catch (ClassNotFoundException e) {
			// we map this to IOException as it is an unlikely case and
			// indicates data corruption
			throw new StorageException("Invalid file contents: "
					+ e.getMessage(), e);
		} catch (IOException e) {
			throw new StorageException("Could not load store: "
					+ e.getMessage(), e);
		} finally {
			FileSystemUtils.close(in);
		}
	}

	/** Removes all data from this store. */
	public synchronized void clear() {
		data.clear();
	}

	/**
	 * Returns statistics on the memory consumption of the store. This operation
	 * is potentially expensive and thus should not be called too often. This
	 * method is not thread-safe and should not be called concurrently to other
	 * store operations.
	 */
	public synchronized String getUsageStatistics(boolean detailed) {
		if (!detailed) {
			return "entries: " + data.size();
		}
		long keyBytes = 0;
		long valueBytes = 0;
		for (Entry<byte[], byte[]> entry : data.entrySet()) {
			keyBytes += entry.getKey().length;
			valueBytes += entry.getValue().length;
		}

		return "entries: " + data.size() + ", keys (byte): " + keyBytes
				+ ", values (byte): " + valueBytes;
	}

	/**
	 * Creates a snapshot of the currently stored data. Only one snapshot is
	 * stored at any time. Calling this a second time creates a new snapshot and
	 * overwrites the old one.
	 */
	public synchronized void createSnapshot() {
		// a shallow copy is sufficient, as the byte arrays are not modified in
		// this class and only cloned arrays are exchanged at the interface.
		snapshot = new TreeMap<>(data);
	}

	/**
	 * Restores the data from the last snapshot. The snapshot is preserved, i.e.
	 * the data can be restored multiple times until a new snapshot is created
	 * using {@link #createSnapshot()}.
	 * 
	 * @throws AssertionError
	 *             if no snapshot has been created before using
	 *             {@link #createSnapshot()}.
	 */
	public synchronized void restoreFromSnapshot() {
		CCSMAssert.isTrue(snapshot != null,
				"Must create a snapshot before restoring!");
		data = new TreeMap<>(snapshot);
	}
}
