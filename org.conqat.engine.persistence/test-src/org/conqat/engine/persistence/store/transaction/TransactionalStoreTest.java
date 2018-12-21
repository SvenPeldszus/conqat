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
package org.conqat.engine.persistence.store.transaction;

import java.io.File;
import java.util.Arrays;

import org.conqat.engine.persistence.store.IStorageSystem;
import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.StorageSystemTestBase;
import org.conqat.engine.persistence.store.mem.InMemoryStorageSystem;
import org.conqat.engine.persistence.store.util.StorageUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Tests the {@link TransactionalStore}.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51583 $
 * @ConQAT.Rating GREEN Hash: 727568985CE651C6F90053435F898239
 */
public class TransactionalStoreTest extends StorageSystemTestBase {

	/** {@inheritDoc} */
	@Override
	protected IStorageSystem openStorage(File baseDir) throws StorageException {
		return new InMemoryStorageSystem(baseDir) {
			/** {@inheritDoc} */
			@Override
			public synchronized IStore openStore(String name) throws StorageException {
				return new TransactionalStore(super.openStore(name));
			}
		};
	}

	/** {@inheritDoc} */
	@Override
	public void testPersistence() {
		// This case can not work as expected, as the base class does not commit
		// the changes
	}

	/** Tests commit and rollback behavior. */
	public void testCommitRollback() throws StorageException {
		TransactionalStore transactionalStore = (TransactionalStore) underlyingStore;

		store.putWithString("abc", StringUtils.stringToBytes("1"));
		assertEquals("1", StringUtils.bytesToString(store.getWithString("abc")));

		transactionalStore.commit();

		// after a commit, rollback does not affect content.
		transactionalStore.rollback();
		assertEquals("1", StringUtils.bytesToString(store.getWithString("abc")));

		PairList<String, byte[]> keysValues = new PairList<String, byte[]>();
		keysValues.add("abc", StringUtils.stringToBytes("2"));
		keysValues.add("def", StringUtils.stringToBytes("3"));
		store.putWithStrings(keysValues);

		assertEquals("2", StringUtils.bytesToString(store.getWithString("abc")));
		transactionalStore.rollback();
		assertEquals("1", StringUtils.bytesToString(store.getWithString("abc")));

		store.putWithStrings(keysValues);
		store.removeWithString("abc");
		assertNull(store.getWithString("abc"));

		assertEquals(Arrays.asList("def"), StorageUtils.listStringKeys(store));
		transactionalStore.rollback();
		assertEquals(Arrays.asList("abc"), StorageUtils.listStringKeys(store));
	}
}
