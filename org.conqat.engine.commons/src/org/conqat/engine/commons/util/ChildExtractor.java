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
package org.conqat.engine.commons.util;

import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51324 $
 * @ConQAT.Rating GREEN Hash: 45A88EE0C45D1E657F676342C4752991
 */
@AConQATProcessor(description = "Returns the child with the given Id. If no such child is found, a ConQATException is thrown.")
public class ChildExtractor extends
		ConQATInputProcessorBase<IRemovableConQATNode> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(attribute = "id", parameter = "child", description = ""
			+ "Id of the child to return.")
	public String id;

	/** {@inheritDoc} */
	@Override
	public IRemovableConQATNode process() throws ConQATException {
		if (!input.hasChildren()) {
			throw new ConQATException("Input has no children.");
		}
		for (IRemovableConQATNode node : input.getChildren()) {
			if (node.getId().equals(id)) {
				return node;
			}
		}
		throw new ConQATException("No child found with id '" + id + "'.");
	}
}