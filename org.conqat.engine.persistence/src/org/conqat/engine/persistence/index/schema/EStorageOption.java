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
package org.conqat.engine.persistence.index.schema;

import org.conqat.engine.persistence.store.util.CompressingStore;

/**
 * Options that can be used to modify the storage layout used below an index.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51716 $
 * @ConQAT.Rating GREEN Hash: DEE1297395C8FF6D7307B9C892468543
 */
public enum EStorageOption {

	/** Compresses the values using the {@link CompressingStore}. */
	COMPRESSED,

	/** Indicates whether a store will be included in the backup */
	BACKUP,

	/** Uses a historizing store. */
	HISTORIZED,

	/** Uses a branching store. */
	BRANCHED,

	/**
	 * Indicates that temporal rollback should never be performed for this
	 * store. This is used for stores that do not manage historized data.
	 */
	NO_ROLLBACK;
}
