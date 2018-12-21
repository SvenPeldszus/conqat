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
package org.conqat.engine.commons.node;

import java.util.List;

/**
 * Interface for values that provide additional values in attributes (i.e.
 * key/value pairs). This can be used to make more complex values in a node more
 * accessible in the output.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51017 $
 * @ConQAT.Rating GREEN Hash: 4764BA1E9C9F25ACF7B7622C74B5A651
 */
public interface IAttributedValue {

	/** Returns the names of attributes available. */
	List<String> getAttributeNames();

	/** Returns the value stored for an attribute. */
	Object getAttributeValue(String attributeName);
}
