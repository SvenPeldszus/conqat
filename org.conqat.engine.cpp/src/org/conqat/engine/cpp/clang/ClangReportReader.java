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
package org.conqat.engine.cpp.clang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.base.ReportReaderBase;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.enums.EnumUtils;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * {@ConQAT.Doc}
 * 
 * The implementation currently ignores the column information but uses only the
 * line of the finding.
 * 
 * More information on Clang can be found online (http://clang.llvm.org/). The
 * Plist format seems to have no documentation. See the test data for examples.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48349 $
 * @ConQAT.Rating GREEN Hash: 354361854B1847CF2F053495181BBD51
 */
@AConQATProcessor(description = "Reads reports from the Clang static analyzer in plist format. "
		+ "To create these files, use the scan-build tool with the '-plist' parameter.")
public class ClangReportReader extends ReportReaderBase {

	/** Name of the catch-all category. */
	private static final String OTHER_CATEGORY_NAME = "Other";

	/** Contains all categories that are used by clang. */
	private static final Set<String> KNOWN_CATEGORIES = new HashSet<>(
			Arrays.asList("ARC Restrictions", "API Misuse (Apple)", "API",
					"Dead store", "Logic error",
					"Memory (Core Foundation/Objective-C)", "Memory Error",
					"Memory Error", "Unix API"));

	/**
	 * Returns the names of the known Clang categories (used as finding groups)
	 * including the {@link #OTHER_CATEGORY_NAME}.
	 */
	public static List<String> getClangCategoryNames() {
		List<String> categories = CollectionUtils.sort(KNOWN_CATEGORIES);
		categories.add(OTHER_CATEGORY_NAME);
		return categories;
	}

	/** {@inheritDoc} */
	@Override
	protected void loadReport(ITextElement report) throws ConQATException {
		TextElementUtils.parseSAX(report, new PlistHandler());
	}

	/** {@inheritDoc} */
	@Override
	protected String obtainRuleDescription(String ruleId) {
		// we need no separate rule descriptions, as the ruleIds are already
		// readable names
		return null;
	}

	/**
	 * SAX handler for parsing Plist files, which are the output format of
	 * Clang. The Plist files consist of nested key/value lists.
	 */
	private class PlistHandler extends DefaultHandler {

		/** Stack of currently open elements. */
		private final Stack<EPlistElement> openElements = new Stack<EPlistElement>();

		/** Stack of keys we are currently in. */
		private final Stack<EPlistKey> openKeys = new Stack<EPlistKey>();

		/** List of files extracted from plist header. */
		private final List<String> files = new ArrayList<String>();

		/** Builder used for aggregating text content. */
		private final StringBuilder builder = new StringBuilder();

		/** Clang category of the finding. */
		private String category;

		/** Description of the finding. */
		private String description;

		/** The index into the {@link #files}. */
		private int fileIndex;

		/** The 1-based line number. */
		private int line;

		/** {@inheritDoc} */
		@Override
		public void error(SAXParseException e) throws SAXException {
			throw e;
		}

		/** {@inheritDoc} */
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) {
			EPlistElement element = EnumUtils.valueOfIgnoreCase(
					EPlistElement.class, localName);
			if (element == null) {
				element = EPlistElement.OTHER;
			}

			openElements.push(element);
			builder.setLength(0);
		}

		/** {@inheritDoc} */
		@Override
		public void characters(char[] ch, int start, int length) {
			builder.append(ch, start, length);
		}

		/** {@inheritDoc} */
		@Override
		public void endElement(String uri, String localName, String qName)
				throws SAXException {
			EPlistElement element = openElements.pop();
			EPlistElement parentElement1 = null;
			if (!openElements.isEmpty()) {
				parentElement1 = openElements.peek();
			}
			EPlistElement parentElement = parentElement1;

			if (!openKeys.isEmpty()) {
				EPlistKey currentKey = openKeys.peek();
				handleElementEnd(element, currentKey, parentElement);
			}
			updateOpenKeys(element, parentElement);
		}

		/** Processes the ending of the given element. */
		private void handleElementEnd(EPlistElement element,
				EPlistKey currentKey, EPlistElement parentElement)
				throws SAXException {
			switch (element) {
			case STRING:
				readStringValue(currentKey, parentElement);
				break;

			case INTEGER:
				readIntValue(currentKey);
				break;

			case DICT:
				if (parentElement == EPlistElement.ARRAY
						&& currentKey == EPlistKey.DIAGNOSTICS) {
					createFinding();
				}
				break;

			default:
				// nothing to do
			}
		}

		/** Read an int value. */
		private void readIntValue(EPlistKey currentKey) {
			int value = Integer.parseInt(builder.toString());
			switch (currentKey) {
			case LINE:
				line = value;
				break;
			case FILE:
				fileIndex = value;
				break;
			}
		}

		/** Reads a string value. */
		private void readStringValue(EPlistKey currentKey,
				EPlistElement parentElement) {
			String value = builder.toString();
			switch (currentKey) {
			case FILES:
				if (parentElement == EPlistElement.ARRAY) {
					files.add(value);
				}
				break;
			case DESCRIPTION:
				description = value;
				break;
			case CATEGORY:
				if (KNOWN_CATEGORIES.contains(value)) {
					category = value;
				} else {
					category = OTHER_CATEGORY_NAME;
				}
				break;
			}
		}

		/** Updates the {@link #openKeys} stack. */
		private void updateOpenKeys(EPlistElement element,
				EPlistElement parentElement) {
			if (element == EPlistElement.KEY) {
				EPlistKey key = EnumUtils.valueOfIgnoreCase(EPlistKey.class,
						builder.toString());
				if (key == null) {
					key = EPlistKey.OTHER;
				}
				openKeys.push(key);
			} else if (parentElement == EPlistElement.DICT) {
				openKeys.pop();
			}
		}

		/** Creates a finding from the values stored in attributes. */
		private void createFinding() throws SAXException {
			try {
				createLineFinding(category, description, files.get(fileIndex),
						line);
			} catch (ConQATException e) {
				throw new SAXException(e);
			}

			// reset values to detect missing content later on
			category = null;
			description = null;
			line = 0;
			fileIndex = 0;
		}
	}
}
