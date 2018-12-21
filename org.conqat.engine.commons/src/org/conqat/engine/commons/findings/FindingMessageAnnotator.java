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
package org.conqat.engine.commons.findings;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.traversal.ETargetNodes;
import org.conqat.engine.commons.traversal.TargetExposedNodeTraversingProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.string.StringUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author hummelb
 * @author $Author: streitel $
 * @version $Rev: 50576 $
 * @ConQAT.Rating RED Hash: 7D2BC2C936286934DB6B507D05532886
 */
@AConQATProcessor(description = "Processor that takes all findings of a report, concatenates their messages and writes these messages to a ConQAT key.")
public class FindingMessageAnnotator extends
		TargetExposedNodeTraversingProcessorBase<IConQATNode> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.FINDING_PARAM_NAME, attribute = ConQATParamDoc.FINDING_KEY_NAME, optional = false, description = ConQATParamDoc.FINDING_KEY_DESC)
	public String findingKey;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.WRITEKEY_NAME, attribute = ConQATParamDoc.WRITEKEY_KEY_NAME, optional = false, description = ConQATParamDoc.WRITEKEY_KEY_DESC)
	public String writeKey;

	/** {@inheritDoc} */
	@Override
	protected ETargetNodes getDefaultTargetNodes() {
		return ETargetNodes.LEAVES;
	}

	/** {@inheritDoc} */
	@Override
	protected void setUp(IConQATNode root) throws ConQATException {
		super.setUp(root);
		NodeUtils.addToDisplayList(root, writeKey);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IConQATNode node) {
		StringBuilder findingMessages = new StringBuilder();

		FindingsList findings = NodeUtils.getFindingsList(node, findingKey);
		// TODO (FS) sorry, but my comment that caused you to add this if
		// statement was not well thought through: previously, the processor
		// would write the empty string to writeKey if findings == null. now
		// nothing is stored there. please check which version is desired. if
		// storing nothing is OK, you can just remove this comment and make the
		// file green. otherwise, please revert to the old version of this
		// method before my half-baked review and make the file green as well
		// :-)
		if (findings == null) {
			return;
		}

		for (Finding finding : findings) {
			if (findingMessages.length() > 0) {
				findingMessages.append(StringUtils.CR);
			}
			findingMessages.append(finding.getMessage());
		}

		node.setValue(writeKey, findingMessages.toString());
	}
}