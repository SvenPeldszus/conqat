/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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

import org.conqat.engine.text.comments.analysis.CommentTestBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Test for metric findings. Tests the {@link TrivialInterfaceCommentAnalysis},
 * {@link UnrelatedInterfaceCommentAnalysis}, {@link LongInlineCommentAnalysis},
 * {@link ShortInlineCommentAnalysis}, and {@link CommentedOutCodeAnalysis},
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51412 $
 * @ConQAT.Rating GREEN Hash: 6989EC4F635E691C78F4EBEBB3580535
 */
public class CommentFindingTest extends CommentTestBase {

	/** Test findings for trivial interface comments. */
	public void testTrivialInterfaceCommentFindings() throws Exception {
		assertFindingList(TrivialInterfaceCommentAnalysis.class,
				"TrivialComments.java",
				"Trivial Member Comment @ TEST/TrivialComments.java:46-46");
		assertFindingList(TrivialInterfaceCommentAnalysis.class,
				"CommentFindings.java",
				"Trivial Member Comment @ TEST/CommentFindings.java:40-40",
				"Trivial Member Comment @ TEST/CommentFindings.java:46-46");
	}

	/** Test findings for unrelated interface comments. */
	public void testUnrelatedInterfaceCommentFindings() throws Exception {
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"BaselineConfiguration.java",
				"Unrelated Member Comment @ TEST/BaselineConfiguration.java:128-128");
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"TrivialComments.java",
				"Unrelated Member Comment @ TEST/TrivialComments.java:31-31",
				"Unrelated Member Comment @ TEST/TrivialComments.java:41-41");
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"CommentFindings.java",
				"Unrelated Member Comment @ TEST/CommentFindings.java:28-28",
				"Unrelated Member Comment @ TEST/CommentFindings.java:36-36");
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"unrelated-comments.cs", ELanguage.CS,
				"Unrelated Member Comment @ TEST/unrelated-comments.cs:27-30",
				"Unrelated Member Comment @ TEST/unrelated-comments.cs:45-48");
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"UnrelatedComments.java", ELanguage.JAVA,
				"Unrelated Member Comment @ TEST/UnrelatedComments.java:5-5");

	}

	/** Test findings for commented out code. */
	public void testCommentedCodeFindings() throws Exception {
		assertFindingList(CommentedOutCodeAnalysis.class,
				"CommentFindings.java",
				"Commented Out Code @ TEST/CommentFindings.java:61-67");
	}

	/** Tests inline comment length findings. */
	public void testInlineCommentFindings() throws Exception {
		assertFindingList(LongInlineCommentAnalysis.class,
				"CommentFindings.java",
				"Long Inline Comment @ TEST/CommentFindings.java:57-57");

		assertFindingList(ShortInlineCommentAnalysis.class,
				"CommentFindings.java",
				"Short Inline Comment @ TEST/CommentFindings.java:53-53");
		assertFindingList(ShortInlineCommentAnalysis.class,
				"InlineJavadocComment.java",
				"Short Inline Comment @ TEST/InlineJavadocComment.java:7-7");
		assertFindingList(ShortInlineCommentAnalysis.class, "inheritdoc.cs",
				ELanguage.CS, "Short Inline Comment @ TEST/inheritdoc.cs:29-29");
	}

	/** Test for use of preprocessor. */
	public void testPreprocessor() throws Exception {
		assertFindingList(CommentedOutCodeAnalysis.class, "Class1.cs",
				ELanguage.CS, "Commented Out Code @ TEST/Class1.cs:20-20");
	}

	/**
	 * Checks that GWT's native JavaScript code is not recognized as commented
	 * code. (CR#6366)
	 */
	public void testGwtCommentedCode() throws Exception {
		assertFindingList(CommentedOutCodeAnalysis.class, "gwt-native.java",
				"Commented Out Code @ TEST/gwt-native.java:6-6");
	}

	/** Tests unrelated comment check for enum literals. */
	public void testEnumLiterals() throws Exception {
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"test-enum.java",
				"Unrelated Member Comment @ TEST/test-enum.java:15-15",
				"Unrelated Member Comment @ TEST/test-enum.java:9-9");
	}

	/** Test for an identifier consisting of a single character */
	public void testSingleCharIdentifier() throws Exception {
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"SingleCharIdentifier.java");
	}

	/** Test for CR#6668 */
	public void testConstant() throws Exception {
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"Constant.java");
	}

	/** Test for CR#6584 */
	public void testCR6584() throws Exception {
		assertFindingList(UnrelatedInterfaceCommentAnalysis.class,
				"UnrelatedCR6584.java");
	}
}
