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

import org.conqat.engine.commons.pattern.PatternTransformationList;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.sourcecode.coverage.ELineCoverage;
import org.conqat.engine.sourcecode.coverage.LineCoverageAnalyzerBase;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 50585 $
 * @ConQAT.Rating GREEN Hash: 39493BBFA11AF049D5479E2257B55827
 */
@AConQATProcessor(description = "Parses test coverage report files generated "
		+ "by gcov (http://gcc.gnu.org/onlinedocs/gcc/Gcov.html) and annotates nodes "
		+ "with line coverage information. This processor can merge overlapping "
		+ "coverage reports.")
public class GcovCoverageAnalyzer extends LineCoverageAnalyzerBase {

	/** Prefix used to mark the source file. */
	private static final String SOURCE_PREFIX = "Source:";

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "path-transformation", attribute = "ref", optional = true, description = ""
			+ "A transformation for mapping uniform paths to the paths as found in the coverage reports.")
	public PatternTransformationList uniformPathTransformation;

	/** Constructor. */
	public GcovCoverageAnalyzer() {
		super(ELanguage.CPP);
	}

	/** {@inheritDoc} */
	@Override
	protected String getQualifiedSourceFileName(ITokenElement element) {
		String uniformPath = element.getUniformPath();
		if (uniformPathTransformation != null) {
			uniformPath = uniformPathTransformation
					.applyTransformation(uniformPath);
		}
		return uniformPath;
	}

	/** {@inheritDoc} */
	@Override
	protected void parseCoverageReport(ITextElement coverageReport)
			throws ConQATException {
		LineCoverageInfo coverageInfo = null;

		for (String line : StringUtils.splitLinesAsList(coverageReport
				.getTextContent())) {
			String[] parts = line.split(":", 3);
			if (parts.length != 3) {
				// ignore all additional lines (branching info, etc)
				continue;
			}

			int lineNumber = Integer.parseInt(parts[1].trim());
			if (lineNumber == 0) {
				if (parts[2].startsWith(SOURCE_PREFIX)) {
					coverageInfo = getOrCreateCoverageInfo(StringUtils
							.stripPrefix(parts[2], SOURCE_PREFIX));
				}
			} else {
				if (coverageInfo == null) {
					throw new ConQATException("Missing '" + SOURCE_PREFIX
							+ "' at start of file!");
				}
				updateCoverageInfo(lineNumber, parts[0].trim(), coverageInfo);
			}
		}
	}

	/** Updates the coverage information for the given line. */
	private void updateCoverageInfo(int lineNumber, String prefix,
			LineCoverageInfo coverageInfo) {
		if (prefix.equals("-")) {
			// not coverable
		} else if (prefix.startsWith("#")) {
			coverageInfo.addLineCoverage(lineNumber, ELineCoverage.NOT_COVERED);
		} else {
			coverageInfo.addLineCoverage(lineNumber,
					ELineCoverage.FULLY_COVERED);
		}
	}
}
