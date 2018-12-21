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

import java.util.List;

import org.conqat.engine.commons.findings.location.LocationAdjuster;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.region.SimpleRegion;

/**
 * Utility methods for dealing with coverage information.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50376 $
 * @ConQAT.Rating GREEN Hash: AB4FE8D0D7B79F306FB48ACE21283800
 */
public class LineCoverageUtils {

	/**
	 * Adjusts line coverage information for a file to a coverage information
	 * for a changed file. The changes to the file are described by a location
	 * adjuster. Due to the nature of coverage, each small change could affect
	 * the coverage globally, hence this is only a rough approximation.
	 * 
	 * @param coverageInfo
	 *            this may be null and then the result is null as well.
	 * @throws ConQATException
	 *             if the line coverage information is invalid (e.g. invalid
	 *             line numbers)
	 */
	public static LineCoverageInfo adjustCoverageInfo(
			LineCoverageInfo coverageInfo, LocationAdjuster adjuster)
			throws ConQATException {
		if (coverageInfo == null) {
			return null;
		}

		LineCoverageInfo adjustedCoverageInfo = new LineCoverageInfo();
		adjustLines(coverageInfo.getUncoveredLines(),
				ELineCoverage.NOT_COVERED, adjustedCoverageInfo, adjuster);
		adjustLines(coverageInfo.getPartiallyCoveredLines(),
				ELineCoverage.PARTIALLY_COVERED, adjustedCoverageInfo, adjuster);
		adjustLines(coverageInfo.getFullyCoveredLines(),
				ELineCoverage.FULLY_COVERED, adjustedCoverageInfo, adjuster);
		return adjustedCoverageInfo;
	}

	/**
	 * Adjusts the given lines using a location adjuster and fills them into the
	 * provided coverage info.
	 * 
	 * @param lines
	 *            the line numbers (one-based)
	 * @throws ConQATException
	 *             if the given line information is invalid
	 */
	private static void adjustLines(List<Integer> lines,
			ELineCoverage coverageType, LineCoverageInfo adjustedCoverageInfo,
			LocationAdjuster adjuster) throws ConQATException {
		for (int line : lines) {
			SimpleRegion adjustedLines = adjuster.adjustLine(line);
			if (adjustedLines == null) {
				continue;
			}
			for (int adjustedLine = adjustedLines.getStart(); adjustedLine <= adjustedLines
					.getEnd(); ++adjustedLine) {
				adjustedCoverageInfo
						.addLineCoverage(adjustedLine, coverageType);
			}
		}
	}
}
