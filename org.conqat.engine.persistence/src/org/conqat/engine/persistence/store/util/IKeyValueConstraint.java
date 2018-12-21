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

import org.conqat.engine.persistence.store.StorageException;

/**
 * A constraint on a key/value pair.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 48867 $
 * @ConQAT.Rating GREEN Hash: 1DC8815EB6ADFB6336D2DDB8A5A24A5A
 */
public interface IKeyValueConstraint {

	/**
	 * Checks the given pait and throws an exception if the constraint is
	 * violated.
	 */
	void check(byte[] key, byte[] value) throws StorageException;
}
