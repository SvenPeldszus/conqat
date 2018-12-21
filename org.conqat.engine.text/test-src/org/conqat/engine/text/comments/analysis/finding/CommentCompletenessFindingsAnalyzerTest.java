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
package org.conqat.engine.text.comments.analysis.finding;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link CommentCompletenessFindingsAnalyzer}.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51766 $
 * @ConQAT.Rating YELLOW Hash: 1F4708E4E9FC45C9C834F783BA6257DE
 */
public class CommentCompletenessFindingsAnalyzerTest extends TokenTestCaseBase {

	/** Tests requirement of comments (any will work). */
	public void testAnyComment() throws ConQATException {
		runAnalysis(false, "CommentCompleteness.java",
				"Interface comment missing @ TEST/CommentCompleteness.java:24-24");
	}

	/** Tests requirement of doc comments. */
	public void testDocComments() throws ConQATException {
		runAnalysis(
				true,
				"CommentCompleteness.java",
				"Interface comment missing @ TEST/CommentCompleteness.java:12-13",
				"Interface comment missing @ TEST/CommentCompleteness.java:24-24",
				"Interface comment missing @ TEST/CommentCompleteness.java:28-28",
				"Interface comment missing @ TEST/CommentCompleteness.java:7-7");
	}

	/** Tests findings on constructors. */
	public void testConstructors() throws ConQATException {
		runAnalysis(false, "CommentedConstructor.java");

		runAnalysis(
				false,
				"UncommentedConstructor.java",
				"Interface comment missing @ TEST/UncommentedConstructor.java:2-2",
				"Interface comment missing @ TEST/UncommentedConstructor.java:4-4");
	}

	/** Tests findings on public constants. */
	public void testConstants() throws ConQATException {
		runAnalysis(false, "CommentedConstants.java",
				"Interface comment missing @ TEST/CommentedConstants.java:27-27");
	}

	/**
	 * Test a file with only rating header, similar to the headers used in our
	 * development.
	 */
	public void testRatingHeader() throws ConQATException {
		runAnalysis(false, "RatingHeader.java",
				"Interface comment missing @ TEST/RatingHeader.java:28-30");
	}

	/** Test file without findings. */
	public void testSingleMethod() throws ConQATException {
		runAnalysis(false, "CommentedSingleMethod.java");
	}

	/** Tests comment completeness for C# files. */
	public void testCsComments() throws ConQATException {
		runAnalysis(
				false,
				"CommentCompleteness.cs",
				ELanguage.CS,
				"Interface comment missing @ TEST/CommentCompleteness.cs:10-10",
				"Interface comment missing @ TEST/CommentCompleteness.cs:12-12",
				"Interface comment missing @ TEST/CommentCompleteness.cs:14-16",
				"Interface comment missing @ TEST/CommentCompleteness.cs:18-19",
				"Interface comment missing @ TEST/CommentCompleteness.cs:28-28",
				"Interface comment missing @ TEST/CommentCompleteness.cs:30-31",
				"Interface comment missing @ TEST/CommentCompleteness.cs:8-9");
	}

	/** Test for override in C# */
	public void testCsOverriddenMethod() throws Exception {
		runAnalysis(true, "(method) & !override", "LocationAdjuster.cs",
				ELanguage.CS);
	}

	/**
	 * Tests comment completeness for C# files with local delegate methods
	 * (CR#6647).
	 */
	public void testCsCommentsWithLocalDelegate() throws ConQATException {
		runAnalysis(false, "CommentCompletenessDelegate.cs", ELanguage.CS,
				"Interface comment missing @ TEST/CommentCompletenessDelegate.cs:11-12");
	}

	/**
	 * Runs the analysis on a test Java file and checks for the expected
	 * findings.
	 */
	private void runAnalysis(boolean requireDocComment, String filename,
			String... expectedFindings) throws ConQATException {
		runAnalysis(requireDocComment, filename, ELanguage.JAVA,
				expectedFindings);
	}

	/**
	 * Runs the analysis on a test file and checks for the expected findings.
	 */
	private void runAnalysis(boolean requireDocComment, String filename,
			ELanguage language, String... expectedFindings)
			throws ConQATException {
		runAnalysis(requireDocComment,
				"(type | method | attribute) & !simple-getter", filename,
				language, expectedFindings);
	}

	/**
	 * Runs the analysis on a test file and checks for the expected findings.
	 */
	private void runAnalysis(boolean requireDocComment,
			String selectorExpression, String filename, ELanguage language,
			String... expectedFindings) throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), language);
		executeProcessor(CommentCompletenessFindingsAnalyzer.class,
				"(input=(ref=", element, "), 'doc-comment'=(require=",
				requireDocComment, "), selector=(expression='"
						+ selectorExpression + "'))");

		assertEquals(StringUtils.concat(expectedFindings, "\n"),
				extractFindingsAsString(element));
	}
}
