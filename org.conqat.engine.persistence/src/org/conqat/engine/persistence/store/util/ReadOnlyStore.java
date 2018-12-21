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
package org.conqat.engine.persistence.store.util;

import java.util.List;

import org.conqat.engine.persistence.store.IStore;
import org.conqat.engine.persistence.store.StorageException;
import org.conqat.engine.persistence.store.base.DelegatingStore;
import org.conqat.lib.commons.collections.PairList;

/**
 * A delegating store that rejects all operations that could modify the store.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 48576 $
 * @ConQAT.Rating GREEN Hash: B1D04ADC40847A4469AFB0E540604E61
 */
public class ReadOnlyStore extends DelegatingStore {

	/** Constructor. */
	public ReadOnlyStore(IStore store) {
		super(store);
	}

	/** {@inheritDoc} */
	@Override
	public void put(byte[] key, byte[] value) throws StorageException {
		denyWriteAccess();
	}

	/** This method denies write access by throwing an exception. */
	private void denyWriteAccess() throws StorageException {
		throw new StorageException("This store is read-only!");
	}

	/** {@inheritDoc} */
	@Override
	public void put(PairList<byte[], byte[]> keysValues)
			throws StorageException {
		denyWriteAccess();
	}

	/** {@inheritDoc} */
	@Override
	public void remove(byte[] key) throws StorageException {
		denyWriteAccess();
	}

	/** {@inheritDoc} */
	@Override
	public void remove(List<byte[]> keys) throws StorageException {
		denyWriteAccess();
	}
}
