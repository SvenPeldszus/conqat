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
package org.conqat.engine.java.junit;

import static org.conqat.lib.commons.assessment.ETrafficLightColor.GREEN;
import static org.conqat.lib.commons.assessment.ETrafficLightColor.RED;
import static org.conqat.lib.commons.assessment.ETrafficLightColor.YELLOW;

import java.util.Collections;
import java.util.List;

import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.sorting.NodeIdComparator;
import org.conqat.engine.commons.traversal.TraversalUtils;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.sourcecode.test.TestResultReaderBase;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;

/**
 * Tests the {@link JUnitReportReader}
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 75C364B8B4A0149B579219A13049E73A
 */
public class JUnitReportReaderTest extends TokenTestCaseBase {

	/** Tests reading of a basic report. */
	public void testRead() throws Exception {
		ITextResource reports = createTextScope(getTestDataDirectory(),
				new String[] { "**.xml" }, null);
		IConQATNode output = (IConQATNode) executeProcessor(
				JUnitReportReader.class, "(input=(ref=", reports, "))");

		List<IConQATNode> leaves = TraversalUtils.listLeavesDepthFirst(output);
		Collections.sort(leaves, NodeIdComparator.INSTANCE);

		assertEquals(6, leaves.size());

		assertNameAndColor(leaves.get(0), "testError", RED);
		assertNameAndColor(leaves.get(1), "testFailure", RED);
		assertNameAndColor(leaves.get(2), "testSlow", GREEN);
		assertNameAndColor(leaves.get(3), "testSuccess", GREEN);
		assertNameAndColor(leaves.get(4), "test1", GREEN);
		assertNameAndColor(leaves.get(5), "test2", YELLOW);
	}

	/** Checks the name and assessment value of a node. */
	private void assertNameAndColor(IConQATNode node, String expectedName,
			ETrafficLightColor expectedColor) {
		assertEquals(expectedName, node.getName());
		assertEquals(new Assessment(expectedColor),
				node.getValue(TestResultReaderBase.ASSESSMENT_KEY));
	}
}
