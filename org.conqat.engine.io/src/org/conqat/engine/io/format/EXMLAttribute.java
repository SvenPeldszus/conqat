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
 * Attribute enumeration for XML serialization format used by this bundle.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51016 $
 * @ConQAT.Rating GREEN Hash: 8142A0D855FF9FFD1A3F732AC5D042DE
 */
public enum EXMLAttribute {

	/** Namespace attribute. */
	xmlns,

	/** Hide root flag of root node. */
	hideRoot,

	/** Id attribute. */
	id,

	/** Key attribute. */
	key,

	/** Type attribute. */
	type;
}