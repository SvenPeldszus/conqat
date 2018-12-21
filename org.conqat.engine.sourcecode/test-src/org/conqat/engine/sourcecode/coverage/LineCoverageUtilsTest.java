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
package org.conqat.engine.sourcecode.coverage;

import org.conqat.engine.commons.findings.location.LocationAdjuster;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Tests the {@link LineCoverageUtils}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50376 $
 * @ConQAT.Rating GREEN Hash: 797C8082B5F9E53C913A230305456033
 */
public class LineCoverageUtilsTest extends CCSMTestCaseBase {

	/** Tests adjustment of covered lines. */
	public void testLineAdjustment() throws ConQATException {
		String originalFile = "abc \n def \n ghi \n jkl \n mno \n pqr";

		LineCoverageInfo coverageInfo = new LineCoverageInfo();
		coverageInfo.addLineCoverage(2, ELineCoverage.NOT_COVERED);
		coverageInfo.addLineCoverage(3, ELineCoverage.PARTIALLY_COVERED);
		coverageInfo.addLineCoverage(4, ELineCoverage.FULLY_COVERED);
		coverageInfo.addLineCoverage(5, ELineCoverage.FULLY_COVERED);

		LocationAdjuster adjuster = new LocationAdjuster(originalFile,
				"abc \n def \n NEW \n ghi \n mno \n pqr");
		assertEquals("Fully covered: 5; partially covered: 4; uncovered: 2",
				LineCoverageUtils.adjustCoverageInfo(coverageInfo, adjuster)
						.toLineString());
	}

}
