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
package org.conqat.engine.commons.statistics;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.util.ConQATInputProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51325 $
 * @ConQAT.Rating GREEN Hash: B5B2652B7C64E2C6C55193414A95ADFC
 */
@AConQATProcessor(description = "Extracts a KeyedData object "
		+ "stored at the root of the tree. If there is no value with the "
		+ "specified key, or the value is not a KeyedData object, a ConQATException is thrown.")
public class KeyedDataExtractor extends ConQATInputProcessorBase<IConQATNode> {

	/** Key for the value. */
	@AConQATFieldParameter(attribute = ConQATParamDoc.READKEY_KEY_NAME, description = ConQATParamDoc.READKEY_DESC, parameter = ConQATParamDoc.READKEY_NAME)
	public String key;

	/** {@inheritDoc} */
	@Override
	public KeyedData<?> process() throws ConQATException {
		Object value = input.getValue(key);

		if (value == null) {
			throw new ConQATException("There is no value with key '" + key
					+ "' stored for the root node.");
		}

		if (value instanceof KeyedData<?>) {
			return (KeyedData<?>) value;
		}
		throw new ConQATException("Value '" + value
				+ "' is not of type KeyedData, was: " + value.getClass());
	}
}