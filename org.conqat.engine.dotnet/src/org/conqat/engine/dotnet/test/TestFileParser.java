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
package org.conqat.engine.dotnet.test;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.sourcecode.test.TestResultReaderBase.ResultIdStrategy;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.xml.sax.SAXException;

/**
 * Base class for parsing test files with a SAX parser.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: AA76A749E90826FC310D5555F55FFB22
 */
public abstract class TestFileParser {

	/** Usual date format for the time of a test run. */
	private final DateFormat dateFormat = new SimpleDateFormat(
			"yyyy-MM-dd'T'HH:mm:ss");

	/** The text element containing test results that will be parsed. */
	private final ITextElement textElement;

	/** The test root the results will be attached to. */
	private TestRoot root;

	/** Strategy for determining test result IDs. */
	private final ResultIdStrategy resultIdStrategy;

	/** Constructor */
	public TestFileParser(ITextElement textElement,
			ResultIdStrategy resultIdStrategy) {
		this.textElement = textElement;
		this.resultIdStrategy = resultIdStrategy;
	}

	/**
	 * Parses the relevant data from the test file and attaches it as a
	 * {@link TestRun} to the provided {@link TestRoot}.
	 * <p>
	 * The parser can only parse one text element. Subsequent calls to
	 * {@link #parse(TestRoot)} will throw an {@link AssertionError}.
	 */
	// TODO (FS) several methods in this class are final. please remove
	public final TestRun parse(TestRoot root) throws ConQATException {
		CCSMAssert.isTrue(this.root == null,
				"The parser has already been used.");
		this.root = root;

		return parseElement(textElement);
	}

	/**
	 * Parses the relevant data from the test file and attaches it as
	 * {@link TestRun} to a new {@link TestRoot} instance.
	 * <p>
	 * The parser can only parse one text element. Subsequent calls to
	 * {@link #parse()} will throw an {@link AssertionError}.
	 */
	public final TestRun parse() throws ConQATException {
		return parse(new TestRoot());
	}

	/** Parses the test results from the text element. */
	protected abstract TestRun parseElement(ITextElement textElement)
			throws ConQATException;

	/** @see #root */
	public TestRoot getRoot() {
		return root;
	}

	/**
	 * Parses the date using the pattern assigned to {@link #dateFormat}.
	 */
	protected Date parseDate(String date) throws SAXException {
		try {
			return dateFormat.parse(date);
		} catch (ParseException e) {
			throw new SAXException("Date has invalid format: " + date, e);
		}
	}

	/** @see #resultIdStrategy */
	protected ResultIdStrategy getResultIdStrategy() {
		return resultIdStrategy;
	}
}
