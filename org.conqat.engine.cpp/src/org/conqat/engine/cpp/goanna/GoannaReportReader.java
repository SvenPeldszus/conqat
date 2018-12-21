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
package org.conqat.engine.cpp.goanna;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.base.ReportReaderBase;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48959 $
 * @ConQAT.Rating GREEN Hash: 3DA5E6108F0899E83619E868A1D25EF8
 */
@AConQATProcessor(description = "Reads reports from the Goanna static analyzer in XML format. "
		+ "To create these files, use the goannamake tool with the '--output-xml' parameter. "
		+ "The reports of Goanna contain absolute paths.")
public class GoannaReportReader extends ReportReaderBase {

	/** Name of the element enclosing a warning. */
	private static final String WARNING_ELEMENT_NAME = "warning";

	/** Name of the element enclosing the line number. */
	private static final String LINE_NUMBER_ELEMENT_NAME = "lineNo";

	/** Name of the element enclosing absolute file name. */
	private static final String FILENAME_ELEMENT_NAME = "absFile";

	/** Name of the element enclosing the rule ID. */
	private static final String RULE_ID_ELEMENT_NAME = "checkName";

	/** Name of the element enclosing the message. */
	private static final String MESSAGE_ELEMENT_NAME = "message";

	/** {@inheritDoc} */
	@Override
	protected void loadReport(ITextElement report) throws ConQATException {
		TextElementUtils.parseSAX(report, new GoannaXmlHandler());
	}

	/** {@inheritDoc} */
	@Override
	protected String obtainRuleDescription(String ruleId) {
		// use rule ID as group name
		return null;
	}

	/** SAX handler for parsing Goanna XML files. */
	private class GoannaXmlHandler extends DefaultHandler {

		/** Builder used for aggregating text content. */
		private final StringBuilder textBuilder = new StringBuilder();

		/** The filename for a finding. */
		private String filename;

		/** The line number for a finding. */
		private int lineNumber;

		/** The rule ID for a finding. */
		private String ruleId;

		/** The message for a finding. */
		private String message;

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) {
			textBuilder.setLength(0);
		}

		/** {@inheritDoc} */
		@Override
		public void characters(char[] ch, int start, int length) {
			textBuilder.append(ch, start, length);
		}

		/** {@inheritDoc} */
		@Override
		public void endElement(String uri, String localName, String qName)
				throws SAXException {
			switch (localName) {
			case FILENAME_ELEMENT_NAME:
				filename = textBuilder.toString();
				break;
			case RULE_ID_ELEMENT_NAME:
				ruleId = textBuilder.toString();
				break;
			case MESSAGE_ELEMENT_NAME:
				message = textBuilder.toString();
				break;
			case LINE_NUMBER_ELEMENT_NAME:
				try {
					lineNumber = Integer.parseInt(textBuilder.toString());
				} catch (NumberFormatException e) {
					throw new SAXException("Invalid line number: "
							+ textBuilder.toString());
				}
				break;
			case WARNING_ELEMENT_NAME:
				try {
					createFinding();
				} catch (ConQATException e) {
					throw new SAXException(e);
				}
				break;
			}
		}

		/** Creates a finding. */
		private void createFinding() throws ConQATException {
			if (filename == null || message == null || ruleId == null
					|| lineNumber <= 0) {
				throw new ConQATException(
						"Had incomplete Goanna report. Information missing for warning block.");
			}

			createLineFinding(ruleId, message, filename, lineNumber);

			filename = null;
			message = null;
			ruleId = null;
			lineNumber = 0;
		}
	}
}
