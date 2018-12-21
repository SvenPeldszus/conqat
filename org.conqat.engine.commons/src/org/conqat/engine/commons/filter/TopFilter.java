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
package org.conqat.engine.commons.filter;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATPipelineProcessorBase;
import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 50108 $
 * @ConQAT.Rating GREEN Hash: CDC383CD3FF8B1AA11A6A57D805E8A67
 */
@AConQATProcessor(description = "This filter works like the SQL 'TOP' directive. "
		+ " For every tree node it includes the specified number of children and "
		+ "removes all others. Hence, filtering is defined by the assigned sorter. "
		+ "If no sorter is assigned, the filtering mechanism is undefined.")
public class TopFilter extends
		ConQATPipelineProcessorBase<IRemovableConQATNode> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "top", attribute = "value", description = "The maximal number of children to include.")
	public int numOfChildren;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.INVERT_NAME, attribute = ConQATParamDoc.INVERT_VALUE_NAME, optional = true, description = ConQATParamDoc.INVERT_PARAM_DOC
			+ " If set to true, this returns every leaf but the top elements.")
	public boolean invert = false;

	/** {@inheritDoc} */
	@Override
	protected void processInput(IRemovableConQATNode input) {
		filterNodes(input);
	}

	/**
	 * This method traverses the a node tree and filters children.
	 */
	private void filterNodes(IRemovableConQATNode element) {
		if (!element.hasChildren()) {
			return;
		}

		IRemovableConQATNode[] children = NodeUtils
				.getRemovableSortedChildren(element);
		for (int childCount = 0; childCount < children.length; ++childCount) {
			IRemovableConQATNode child = children[childCount];
			if (shouldBeRemoved(childCount)) {
				child.remove();
			} else {
				filterNodes(child);
			}
		}
	}

	/** Returns whether the n-th child (0-indexed) should be removed. */
	private boolean shouldBeRemoved(int nthChild) {
		if (invert) {
			return nthChild < numOfChildren;
		}
		return nthChild >= numOfChildren;
	}

}