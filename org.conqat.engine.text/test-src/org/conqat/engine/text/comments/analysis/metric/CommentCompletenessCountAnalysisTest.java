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
package org.conqat.engine.text.comments.analysis.metric;

import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link CommentCompletenessCountAnalysis}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47867 $
 * @ConQAT.Rating GREEN Hash: 82CF4017DD0681983D490C84406703A5
 */
public class CommentCompletenessCountAnalysisTest extends TokenTestCaseBase {

	/** Test API Coverage. */
	public void testAPICoverage() throws Exception {
		String selectorExpression = "public & method";
		assertCoverage("CompletelyCommentedClass.java", selectorExpression, 3,
				3);
		assertCoverage("HalfCommentedClass.java", selectorExpression, 2, 4);
		assertCoverage("ClassWithNoComments.java", selectorExpression, 0, 2);
	}

	/** Test Type Coverage. */
	public void testTypeCoverage() throws Exception {
		String selectorExpression = "public & type";
		assertCoverage("CompletelyCommentedClass.java", selectorExpression, 1,
				1);
		assertCoverage("HalfCommentedClass.java", selectorExpression, 1, 1);
		assertCoverage("ClassWithNoComments.java", selectorExpression, 0, 1);
	}

	/**
	 * Test the coverage for a given selector expression with the number of
	 * expected comments found and the overall number of selected entties found.
	 */
	private void assertCoverage(String filename, String selectorExpression,
			int expectedNumberOfComments, int expectedNumberOfSelectedEntities)
			throws Exception {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), ELanguage.JAVA);
		executeProcessor(CommentCompletenessCountAnalysis.class,
				"(input=(ref=", element, "), selector=(expression='",
				selectorExpression,
				"'), 'commented-count'=(key='commented'), 'overall-count'=(key='overall'))");

		assertEquals(expectedNumberOfComments,
				((Integer) element.getValue("commented")).intValue());
		assertEquals(expectedNumberOfSelectedEntities,
				((Integer) element.getValue("overall")).intValue());
	}
}
