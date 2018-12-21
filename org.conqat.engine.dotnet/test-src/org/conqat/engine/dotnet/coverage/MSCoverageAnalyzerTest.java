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
package org.conqat.engine.dotnet.coverage;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests for the {@link MSCoverageAnalyzer}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49139 $
 * @ConQAT.Rating GREEN Hash: 2999060851DFC29CBC4553953BBCD529
 */
public class MSCoverageAnalyzerTest extends TokenTestCaseBase {

	/** Basic test for analyzing coverage data. */
	public void test() throws Exception {

		ITokenResource sourceScope = createResourceHierarchyFromZip(
				useTestFile("CodeCoverageTestApp.zip"), ELanguage.CS);

		runCoverageAnalysis(sourceScope, "CoverageReport.xml");

		LineCoverageInfo coverageInfo = (LineCoverageInfo) sourceScope
				.getChildren()[0].getChildren()[0]
				.getValue(LineCoverageAnalyzerBase.COVERAGE_INFO_KEY);
		assertEquals("[8, 9, 10, 11, 12, 13, 14, 17, 18, 21, 22, 24, 25, 33]",
				coverageInfo.getFullyCoveredLines().toString());
		assertEquals("[]", coverageInfo.getPartiallyCoveredLines().toString());
		assertEquals("[27, 28, 29, 31]", coverageInfo.getUncoveredLines()
				.toString());
		assertEquals(.77d, coverageInfo.getCoverageRatio(), .01d);
	}

	/**
	 * Executes the coverage analysis for the given scope and coverage file
	 * within the test data folder
	 */
	private void runCoverageAnalysis(ITokenResource sourceScope,
			String coverageFile) throws ConQATException {
		ITokenElement coverageReport = createTokenElement(
				useCanonicalTestFile(coverageFile), ELanguage.XML);
		executeProcessor(
				MSCoverageAnalyzer.class,
				"(input=(ref=",
				sourceScope,
				"), 'coverage-reports'=(scope=",
				coverageReport,
				"), 'report-source'=(root='c:/Users/ny86183a/workspace/org.conqat.mscoverage/test-data/CodeCoverageTestApp'))");
	}

}
