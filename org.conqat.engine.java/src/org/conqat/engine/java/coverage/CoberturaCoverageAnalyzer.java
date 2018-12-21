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

import java.io.IOException;
import java.io.StringReader;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.sourcecode.coverage.ELineCoverage;
import org.conqat.engine.sourcecode.coverage.LineCoverageInfo;
import org.conqat.lib.commons.xml.XMLUtils;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 50494 $
 * @ConQAT.Rating GREEN Hash: 93C13881CECD04948B94AF51FC0CE01C
 */
@AConQATProcessor(description = "Parses test coverage report files generated "
		+ "by Cobertura (http://cobertura.github.io/cobertura/) and annotates nodes "
		+ "with line coverage information. This processor can merge overlapping "
		+ "coverage reports.")
public class CoberturaCoverageAnalyzer extends JavaCoverageAnalyzerBase {

	/** {@inheritDoc} */
	@Override
	protected void parseCoverageReport(ITextElement coverageReport)
			throws ConQATException {
		try {
			InputSource input = new InputSource(new StringReader(
					coverageReport.getTextContent()));
			XMLUtils.parseSAX(input, new CoberturaReportHandler());
		} catch (SAXException | IOException e) {
			throw new ConQATException("Couldn't read coverage report: "
					+ e.getMessage(), e);
		}
	}

	/** SAX handler for parsing Cobertura reports. */
	private class CoberturaReportHandler extends CoberturaReportHandlerBase {

		/** The current line coverage information */
		private LineCoverageInfo currentLineCoverage;

		/** Flag indicating if we are within a methods tag */
		private boolean inMethods = false;

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) {

			switch (qName) {
			case CLASS_ELEMENT:
				currentLineCoverage = getOrCreateCoverageInfo(attributes
						.getValue(FILENAME_ATTRIBUTE));
				break;
			case METHODS_ELEMENT:
				inMethods = true;
				break;
			case LINE_ELEMENT:
				if (!inMethods) {
					updateLineCoverage(attributes);
				}
				break;

			default:
				// ignore other elements
			}

		}

		/**
		 * Updates the line coverage from the given attributes of a line
		 * element.
		 */
		private void updateLineCoverage(Attributes attributes) {
			int lineNumber = Integer.valueOf(attributes
					.getValue(LINE_NUMBER_ATTRIBUTE));
			int hits = Integer.valueOf(attributes.getValue(HITS_ATTRIBUTE));
			if (hits == 0) {
				currentLineCoverage.addLineCoverage(lineNumber,
						ELineCoverage.NOT_COVERED);
				return;
			}

			boolean branch = Boolean.valueOf(attributes
					.getValue(BRANCH_ATTRIBUTE));
			if (!branch) {
				currentLineCoverage.addLineCoverage(lineNumber,
						ELineCoverage.FULLY_COVERED);
			} else {
				String conditionCoverage = attributes
						.getValue(CONDITION_COVERAGE_ATTRIBUTE);
				if (conditionCoverage.contains("100%")) {
					currentLineCoverage.addLineCoverage(lineNumber,
							ELineCoverage.FULLY_COVERED);
				} else {
					currentLineCoverage.addLineCoverage(lineNumber,
							ELineCoverage.PARTIALLY_COVERED);
				}
			}
		}

		/** {@inheritDoc} */
		@Override
		public void endElement(String uri, String localName, String qName) {
			if (qName.equals(METHODS_ELEMENT)) {
				inMethods = false;
			}
		}

	}

}
