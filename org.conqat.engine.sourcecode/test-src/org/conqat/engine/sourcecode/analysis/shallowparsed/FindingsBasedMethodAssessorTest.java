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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import org.conqat.engine.commons.findings.Finding;
import org.conqat.engine.commons.findings.FindingGroup;
import org.conqat.engine.commons.findings.util.FindingUtils;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.util.ResourceUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link FindingsBasedMethodAssessor}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49583 $
 * @ConQAT.Rating GREEN Hash: D3616C3B5D1FFCD07452768DDFBE320E
 */
public class FindingsBasedMethodAssessorTest extends TokenTestCaseBase {

	/** Tests LOC based aggregation. */
	public void testLOC() throws ConQATException {
		assertEquals("RED: 7, ORANGE: 6, YELLOW: 5, GREEN: 3",
				runAssessor(false));
	}

	/** Tests SLOC based aggregation. */
	public void testSLOC() throws ConQATException {
		assertEquals("RED: 7, ORANGE: 5, YELLOW: 3, GREEN: 3",
				runAssessor(true));
	}

	/** Test case for a file that can not be parsed CR#6438. */
	public void testNonParsableFile() throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile("CR6438.java"), ELanguage.JAVA);

		executeProcessor(
				ShallowParsedStatementNestingDepthAnalyzer.class,
				"(input=(ref=",
				element,
				"), findings=(threshold=5., key=findings, color=RED), "
						+ "findings=(threshold=3., key=findings, color=YELLOW))");
		executeProcessor(
				FindingsBasedMethodAssessor.class,
				"(input=(ref=",
				element,
				"), write=(key=assessment), findings=(key=findings), 'sloc-based'=(value=true))");

		// we do not check anything; we are only interested that no exceptions
		// are thrown.
	}

	/** Runs the assessor and returns the assessment as string. */
	private String runAssessor(boolean slocBased) throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile("findingsbasedmethodassessor.js"),
				ELanguage.JAVASCRIPT);

		FindingGroup group = NodeUtils.getFindingReport(element)
				.getOrCreateCategory("cat").getOrCreateFindingGroup("group");

		// first method is YELLOW
		addFinding(element, group, 8, ETrafficLightColor.YELLOW);

		// second method is ORANGE, but not its children (which are GREEN
		addFinding(element, group, 17, ETrafficLightColor.ORANGE);

		// everything in method "ugly" is RED
		addFinding(element, group, 45, ETrafficLightColor.RED);

		executeProcessor(FindingsBasedMethodAssessor.class, "(input=(ref=",
				element,
				"), write=(key=assessment), findings=(key=findings), 'sloc-based'=(value="
						+ slocBased + "))");

		return extractAssessmentString(element);
	}

	/** Extracts the assessment as a string. */
	private String extractAssessmentString(ITokenElement element) {
		Object value = element.getValue("assessment");
		assertTrue(value instanceof Assessment);
		return toAssessmentString((Assessment) value);
	}

	/** Returns a test-friendly string representation of the assessment. */
	private String toAssessmentString(Assessment assessment) {
		StringBuilder builder = new StringBuilder();
		for (ETrafficLightColor color : ETrafficLightColor.values()) {
			int frequency = assessment.getColorFrequency(color);
			if (frequency > 0) {
				if (builder.length() > 0) {
					builder.append(", ");
				}
				builder.append(color + ": " + frequency);
			}
		}
		return builder.toString();
	}

	/** Adds a finding at given line with given color. */
	private void addFinding(ITokenElement element, FindingGroup group,
			int filteredLineNumber, ETrafficLightColor color)
			throws ConQATException {
		Finding finding = ResourceUtils.createAndAttachFindingForFilteredLine(
				group, "message", element, filteredLineNumber, "findings");
		FindingUtils.setFindingColor(finding, color);
	}
}
