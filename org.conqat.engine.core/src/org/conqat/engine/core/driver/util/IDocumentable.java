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
package org.conqat.engine.core.driver.util;

/**
 * Interface for marking objects that allow the addition of documentation.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48607 $
 * @ConQAT.Rating GREEN Hash: D2E73D8084A7BF3BC7B3FD4313F4D119
 */
public interface IDocumentable extends IDocumented {

	/** Sets the documentation for the object. */
	public void setDoc(String doc);

}