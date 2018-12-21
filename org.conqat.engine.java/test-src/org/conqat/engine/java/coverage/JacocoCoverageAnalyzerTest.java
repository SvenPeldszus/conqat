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
package org.conqat.engine.java.coverage;

import java.util.HashMap;
import java.util.Map;

import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementUtils;

/**
 * Tests for the {@link JacocoCoverageAnalyzer}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48986 $
 * @ConQAT.Rating GREEN Hash: D49AECF7822CBEDB45316563AF961CDA
 */
public class JacocoCoverageAnalyzerTest extends JavaCoverageAnalyzerTestBase {

	/** {@inheritDoc} */
	@Override
	protected Class<? extends JavaCoverageAnalyzerBase> getCoverageAnalyzerClass() {
		return JacocoCoverageAnalyzer.class;
	}

	/** {@inheritDoc} */
	@Override
	protected String getSimpleReportFile() {
		return "jacoco-report.xml";
	}

	/** Smoke test with 'real-world' data for the ConQAT commons */
	public void testCommons() throws Exception {

		ITokenResource sourceScope = createJavaResourceHierarchyFromZip(useTestFile("commons-src.zip"));

		Map<String, ITokenResource> tokenResourcesByPath = new HashMap<>();
		for (ITokenElement resource : TokenElementUtils
				.listTokenElements(sourceScope)) {
			tokenResourcesByPath.put(resource.getUniformPath(), resource);
		}

		runCoverageAnalysis(sourceScope, "commons-coverage.xml");

		LineCoverageInfo coverageInfo = (LineCoverageInfo) tokenResourcesByPath
				.get("src/org/conqat/lib/commons/algo/Diff.java").getValue(
						LineCoverageAnalyzerBase.COVERAGE_INFO_KEY);

		assertEquals(.84d, coverageInfo.getCoverageRatio(), .01d);
		assertEquals(
				"[88, 89, 90, 91, 92, 94, 95, 96, 97, 98, 99, 103, 108, 109, 111, "
						+ "114, 116, 117, 118, 119, 121, 123, 125, 126, 128, 130, 131, 133, "
						+ "134, 136, 137, 138, 147, 149, 150, 152, 153, 154, 155, 156, 159, "
						+ "160, 162, 163, 166, 167, 168, 169, 171, 173, 176, 177, 179, 180, "
						+ "184, 236, 256, 280, 306, 339, 340, 341, 342, 343, 344, 351, 387, "
						+ "397, 405, 406, 408, 409, 410, 412, 413, 414, 415, 416, 418, 419, "
						+ "422, 423, 424, 425, 426, 428, 431, 432, 433, 434, 437]",
				coverageInfo.getFullyCoveredLines().toString());
		assertEquals("[110, 129, 148, 172, 386, 396]", coverageInfo
				.getPartiallyCoveredLines().toString());
		assertEquals(
				"[200, 220, 356, 361, 366, 377, 443, 444, 445, 446, 447, 449, 451, 453]",
				coverageInfo.getUncoveredLines().toString());

	}

}
