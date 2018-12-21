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
package org.conqat.engine.cpp.coverage;

import java.util.Collections;
import java.util.List;

import org.conqat.engine.commons.sorting.NodeIdComparator;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementUtils;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link GcovCoverageAnalyzer}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48760 $
 * @ConQAT.Rating GREEN Hash: 003F7ADEE30D2CA138539F519094679B
 */
public class GcovCoverageAnalyzerTest extends TokenTestCaseBase {

	/** Tests coverage information. */
	public void testCoverage() throws ConQATException {
		ITokenResource sourceScope = createResourceHierarchyFor(
				useTestFile("gcov"), ELanguage.CPP, "**.cpp", "**.h");
		ITextResource coverageScope = createTextScope(useTestFile("gcov"),
				new String[] { "**.gcov" }, null);
		executeProcessor(GcovCoverageAnalyzer.class, "(input=(ref=",
				sourceScope, "), 'coverage-reports'=(scope=", coverageScope,
				"))");

		List<ITokenElement> sourceElements = TokenElementUtils
				.listTokenElements(sourceScope);
		Collections.sort(sourceElements, NodeIdComparator.INSTANCE);

		assertEquals(3, sourceElements.size());
		assertNameAndCoverage(sourceElements.get(0), "include/test.h",
				"Fully covered: 14,15,16,22,23,24; partially covered: ; uncovered: 18,19,20");
		assertNameAndCoverage(sourceElements.get(1), "module/test.cpp",
				"Fully covered: 3,4,5,6,10; partially covered: ; uncovered: 8");
		assertNameAndCoverage(sourceElements.get(2), "test.cpp",
				"Fully covered: 3,4,5,11,12,13,14; partially covered: ; uncovered: 7,8,9");
	}

	/** Checks an element's name and coverage info. */
	private void assertNameAndCoverage(ITokenElement element,
			String expectedPath, String expectedCoverage) {
		assertEquals(expectedPath, element.getUniformPath());
		LineCoverageInfo coverageInfo = (LineCoverageInfo) element
				.getValue(LineCoverageAnalyzerBase.COVERAGE_INFO_KEY);
		assertNotNull(coverageInfo);
		assertEquals(expectedCoverage, coverageInfo.toLineString());
	}
}
