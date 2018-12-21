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
package org.conqat.engine.persistence.store;

import org.conqat.engine.persistence.index.schema.SchemaAwareStorageSystem;

/**
 * The storage systems is the central instance and allows access to several
 * named stores. Each instance of a storage system created has to be closed
 * explicitly by calling the {@link #close()} method to prevent data loss and/or
 * corruption. After closing, no other methods on this storage system and
 * {@link IStore}s retrieved from it may be called.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47446 $
 * @ConQAT.Rating GREEN Hash: BAE143E45D4FDD4587BE6E9EE1FC099B
 */
public interface IStorageSystem {

	/**
	 * Opens the store with the given name. Note that this returns a "raw" view
	 * on the store. If using a {@link SchemaAwareStorageSystem}, be sure to
	 * open the store via the schema to apply all view options.
	 */
	IStore openStore(String name) throws StorageException;

	/** Removes the store of the given name. */
	void removeStore(String storeName) throws StorageException;

	/** Closes the storage system. */
	void close() throws StorageException;
}