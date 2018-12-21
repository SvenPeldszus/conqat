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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.StorageSystemBase;
import org.conqat.engine.persistence.store.util.IKeyValueConstraint;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.string.StringUtils;

/**
 * In memory implementation of a storage system. The store also supports
 * persistence, if a suitable directory is provided. So, if the directory
 * exists, data is read from this directory. Data is only persisted when storage
 * is {@link #close()}d.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51702 $
 * @ConQAT.Rating YELLOW Hash: CEF7DEA600C4D93E6FD7835AE8ECC6C9
 */
public class InMemoryStorageSystem extends StorageSystemBase {

	/** The stores available. */
	private Map<String, InMemoryStore> stores = new HashMap<String, InMemoryStore>();

	/** A snapshot of the available stores. */
	private Map<String, InMemoryStore> snapshot;

	/**
	 * If this is not <code>null</code>, the contents of the individual stores
	 * will be persisted in this directory.
	 */
	private final File persistenceDirectory;

	/**
	 * Constraints to use for put operations. This is shared with all stores
	 * created by this storage system and is live, i.e. changes to this will
	 * affect stores immediately.
	 */
	private final List<IKeyValueConstraint> putConstraints = new ArrayList<>();

	/**
	 * Constructor.
	 * 
	 * @param persistenceDirectory
	 *            if this is not <code>null</code>, the contents of the
	 *            individual stores will be persisted in this directory.
	 */
	public InMemoryStorageSystem(File persistenceDirectory)
			throws StorageException {
		this.persistenceDirectory = persistenceDirectory;

		if (persistenceDirectory != null) {
			ensureStorageDirectory(persistenceDirectory);
		}
	}

	/** {@inheritDoc} */
	@Override
	public synchronized IStore openStore(String name) throws StorageException {
		InMemoryStore store = stores.get(name);
		if (store == null) {
			store = new InMemoryStore(putConstraints);
			if (persistenceDirectory != null) {
				File storeFile = new File(persistenceDirectory, name);
				if (storeFile.canRead()) {
					store.loadFromFile(storeFile);
				}
			}
			stores.put(name, store);
		}
		return store;
	}

	/** {@inheritDoc} */
	@Override
	public synchronized void removeStore(String storeName) {
		stores.remove(storeName);
		if (persistenceDirectory != null) {
			File storeFile = new File(persistenceDirectory, storeName);
			// we do not care if the file is not deleted
			storeFile.delete();
		}
	}

	/** {@inheritDoc}. */
	@Override
	public synchronized void close() throws StorageException {
		if (persistenceDirectory == null) {
			return;
		}

		for (Entry<String, InMemoryStore> entry : stores.entrySet()) {
			entry.getValue().dumpToFile(
					new File(persistenceDirectory, entry.getKey()));
		}
	}

	/** Removes all contents of this storage system. */
	public synchronized void clear() {
		for (InMemoryStore store : stores.values()) {
			store.clear();
		}
	}

	/**
	 * Adds a constraint to be checked by each put operation on this storage
	 * system's stores. This is live, i.e. change sto this will affect stores
	 * immediately.
	 */
	public synchronized void addPutConstraint(IKeyValueConstraint constraint) {
		putConstraints.add(constraint);
	}

	/**
	 * Returns statistics on the memory consumption of the storage system. This
	 * operation is potentially expensive and thus should not be called too
	 * often. This method is not thread-safe and should not be called
	 * concurrently to other store operations.
	 */
	public synchronized String getUsageStatistics(boolean detailed) {
		StringBuilder sb = new StringBuilder();
		for (Entry<String, InMemoryStore> entry : stores.entrySet()) {
			sb.append(entry.getKey() + ": "
					+ entry.getValue().getUsageStatistics(detailed)
					+ StringUtils.CR);
		}
		return sb.toString();
	}

	/**
	 * Creates a snapshot of the currently stored data. Only one snapshot is
	 * stored at any time. Calling this a second time creates a new snapshot and
	 * overwrites the old one.
	 * <p>
	 * This will also create a snapshot for all contained stores.
	 */
	public synchronized void createSnapshot() {
		snapshot = new HashMap<>(stores);
		for (InMemoryStore store : snapshot.values()) {
			store.createSnapshot();
		}
	}

	/**
	 * Restores the data from the last snapshot. The snapshot is preserved, i.e.
	 * the data can be restored multiple times until a new snapshot is created
	 * using {@link #createSnapshot()}.
	 * <p>
	 * This will restore all contained stores.
	 * 
	 * @throws AssertionError
	 *             if no snapshot has been created before using
	 *             {@link #createSnapshot()}.
	 */
	public synchronized void restoreFromSnapshot() {
		CCSMAssert.isTrue(hasSnapshot(),
				"Must create a snapshot before restoring!");

		// this is sufficient as all stores not in the snapshot will be no
		// longer reachable then and garbage collected.
		stores = new HashMap<>(snapshot);
		for (InMemoryStore store : stores.values()) {
			store.restoreFromSnapshot();
		}
	}

	/** Returns whether this storage system has a snapshot. */
	public synchronized boolean hasSnapshot() {
		return snapshot != null;
	}
}
