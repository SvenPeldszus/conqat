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

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.engine.sourcecode.coverage.ELineCoverage;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.lib.commons.string.StringUtils;
import org.xml.sax.Attributes;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: goeb $
 * @version $Rev: 51571 $
 * @ConQAT.Rating GREEN Hash: 6AAA6929C3BB2465C5707D36F8FDCFBA
 */
@AConQATProcessor(description = "Parses test coverage report files generated "
		+ "by JaCoCo (http://www.eclemma.org/jacoco/) and annotates nodes "
		+ "with line coverage information. This processor can merge overlapping "
		+ "coverage reports.")
public class JacocoCoverageAnalyzer extends JavaCoverageAnalyzerBase {

	/** {@inheritDoc} */
	@Override
	protected void parseCoverageReport(ITextElement coverageReport)
			throws ConQATException {
		TextElementUtils.parseSAX(coverageReport, new JacocoReportHandler());
	}

	/** SAX handler for parsing Jacoco reports. */
	private class JacocoReportHandler extends JaCoCoReportHandlerBase {

		/** The current package */
		private String currentPackage;

		/** The current line coverage information */
		private LineCoverageInfo currentLineCoverage;

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) {

			switch (qName) {
			case PACKAGE_ELEMENT:
				currentPackage = attributes.getValue(NAME_ATTRIBUTE);
				break;
			case SOURCEFILE_ELEMENT:
				String sourceFile = StringUtils.addPrefix(attributes.getValue(NAME_ATTRIBUTE), "/",
						currentPackage);
				currentLineCoverage = getOrCreateCoverageInfo(sourceFile);
				break;
			case LINE_ELEMENT:
				int lineNumber = Integer.valueOf(attributes
						.getValue(LINE_NUMBER_ATTRIBUTE));
				int missedInstructions = Integer.valueOf(attributes
						.getValue(MISSED_INSTRUCTIONS_ATTRIBUTE));
				int coveredInstructions = Integer.valueOf(attributes
						.getValue(COVERED_INSTRUCTIONS_ATTRIBUTE));
				int missedBranches = Integer.valueOf(attributes
						.getValue(MISSED_BRANCHES_ATTRIBUTE));
				updateLineCoverage(lineNumber, missedInstructions,
						coveredInstructions, missedBranches);
				break;
			default:
				// ignore other elements
			}
		}

		/**
		 * Updates the line coverage in {@link #currentLineCoverage} for the
		 * given line according to the given data for missed and covered
		 * instructions/branches.
		 */
		protected void updateLineCoverage(int lineNumber,
				int missedInstructions, int coveredInstructions,
				int missedBranches) {
			if (coveredInstructions > 0 && missedInstructions == 0
					&& missedBranches == 0) {
				currentLineCoverage.addLineCoverage(lineNumber,
						ELineCoverage.FULLY_COVERED);
			} else if (coveredInstructions > 0) {
				currentLineCoverage.addLineCoverage(lineNumber,
						ELineCoverage.PARTIALLY_COVERED);
			} else if (coveredInstructions == 0
					&& (missedInstructions > 0 || missedBranches > 0)) {
				currentLineCoverage.addLineCoverage(lineNumber,
						ELineCoverage.NOT_COVERED);
			}
			// else: uncoverable line; no info stored
		}
	}
}
