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
package org.conqat.engine.java.junit;

import java.util.regex.Pattern;

import org.conqat.engine.commons.node.StringSetNode;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.engine.sourcecode.test.TestResultReaderBase;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.string.StringUtils;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 031D1ED5DBFDB2568731AA6017EA7645
 */
@AConQATProcessor(description = "Reads JUnit XML reports and creates a node hierarchiy from the test cases.")
public class JUnitReportReader extends TestResultReaderBase<StringSetNode> {

	/** Id of the test root node. */
	private static final String ROOT_ID = "<test-root>";

	/** Parameter object for determining the node ID separator. */
	@AConQATParameterObject
	public ResultIdStrategy idStrategy = new ResultIdStrategy();

	/** The element for test cases. */
	private static final String TESTCASE_ELEMENT = "testcase";

	/** Constructor. */
	public JUnitReportReader() {
		super(new StringSetNode(ROOT_ID));
	}

	/** {@inheritDoc} */
	@Override
	protected void parseTextElement(ITextElement textElement)
			throws ConQATException {
		TextElementUtils.parseSAX(textElement, new JUnitSaxHandler());
	}

	/**
	 * Inserts a test case node into the root node. This creates inner nodes as
	 * required.
	 */
	private void insertTestCase(StringSetNode node) {
		StringSetNode parent = createNodeHierarchy(root, node.getId());

		if (parent.getNamedChild(node.getName()) != null) {
			getLogger().warn(
					"Duplicate test cases with name " + node.getName()
							+ " at parent " + parent.getId()
							+ ". Ignoring all but one.");
		} else {
			parent.addChild(node);
		}
	}

	/**
	 * Creates the node hierarchy with the given id under a parent. The
	 * separator defined by {@link #idStrategy} is used to separate parent nodes
	 * from the passed id.
	 */
	// TODO (FS) please document the return type
	private StringSetNode createNodeHierarchy(StringSetNode parent, String id) {
		String parentId = parent.getId();
		if (parentId.equals(ROOT_ID)) {
			parentId = StringUtils.EMPTY_STRING;
		}

		String separator = idStrategy.getIdSeparator();
		String[] parts = id.split(Pattern.quote(separator), 2);
		if (parts.length < 2) {
			return parent;
		}

		String childName = parts[0];
		StringSetNode child = parent.getNamedChild(childName);
		if (child == null) {
			child = new StringSetNode(parentId + separator + childName,
					childName);
			parent.addChild(child);
		}

		return createNodeHierarchy(child, parts[1]);
	}

	/** Sets the color and result of a test case. */
	private static void setResult(StringSetNode testCase,
			ETrafficLightColor color, String result) {
		testCase.setValue(ASSESSMENT_KEY, new Assessment(color));
		testCase.setValue(RESULT_KEY, result);
	}

	/** The SAX handler for parsing JUnit XML files. */
	private class JUnitSaxHandler extends DefaultHandler {

		/** The node for the current test case (if any). */
		private StringSetNode currentTestCase;

		/** Collected text content of an element. */
		private final StringBuilder currentText = new StringBuilder();

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) throws SAXException {

			currentText.setLength(0);

			if (TESTCASE_ELEMENT.equals(localName)) {
				enterTestCase(attributes);
			}
		}

		/** {@inheritDoc} */
		@Override
		public void endElement(String uri, String localName, String qName)
				throws SAXException {
			switch (localName) {
			case TESTCASE_ELEMENT:
				currentTestCase = null;
				break;
			case "failure":
				handleFailedTest("Failure");
				break;
			case "error":
				handleFailedTest("Error");
				break;
			case "skipped":
				markSkipped();
				break;
			}
		}

		/**
		 * Marks the {@link #currentTestCase} as failed. The given prefix is
		 * appended before the test outcome ({@link #currentText}).
		 */
		private void handleFailedTest(String prefix) throws SAXException {
			if (currentTestCase == null) {
				throw new SAXException(
						"Encountered failure or error element outside of testcase element!");
			}
			setResult(currentTestCase, ETrafficLightColor.RED, prefix + ":"
					+ StringUtils.CR + currentText.toString());
		}

		/** Handles the case of entering a test case element. */
		private void enterTestCase(Attributes attributes) throws SAXException {
			if (currentTestCase != null) {
				throw new SAXException("Not expecting nested test cases!");
			}

			String name = attributes.getValue("name");
			String className = attributes.getValue("classname");
			String timeString = attributes.getValue("time");
			String id = idStrategy.determineId(className, name);

			currentTestCase = new StringSetNode(id, name);
			currentTestCase.setValue(EXECUTION_TIME_KEY,
					Double.parseDouble(timeString));

			if (Boolean.valueOf(attributes.getValue("ignored"))) {
				markSkipped();
			} else {
				setResult(currentTestCase, ETrafficLightColor.GREEN, "Passed");
			}

			insertTestCase(currentTestCase);
		}

		/** Marks the current test case as "skipped". */
		private void markSkipped() {
			setResult(currentTestCase, ETrafficLightColor.YELLOW, "Skipped");
		}

		/** {@inheritDoc} */
		@Override
		public void characters(char[] ch, int start, int length) {
			if (currentTestCase != null) {
				currentText.append(ch, start, length);
			}
		}

		/** {@inheritDoc} */
		@Override
		public void error(SAXParseException e) throws SAXException {
			throw e;
		}
	}
}
