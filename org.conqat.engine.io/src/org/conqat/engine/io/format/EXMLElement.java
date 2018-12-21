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
package org.conqat.engine.io.format;

/**
 * Element enumeration for XML serialization format used by this bundle.
 * 
 * @author Florian Deissenboeck
 * @author $Author: goeb $
 * @version $Rev: 47792 $
 * @ConQAT.Rating GREEN Hash: 1176BE9C7F44D4F4BC2D6E55AD8A84A1
 */
public enum EXMLElement {
	/** Node element. */
	node,

	/** Result (root) element. */
	result,

	/** Key element. */
	key,

	/** Description element. */
	description,

	/** Value element. */
	value,

	/** Element for collection values. */
	collection,

	/** Collection item element. */
	item;
}