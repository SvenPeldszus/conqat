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

import org.conqat.engine.text.comments.analysis.CommentTestBase;

/**
 * Test for comment metrics, i.e. the comment character distribution.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49811 $
 * @ConQAT.Rating GREEN Hash: 297A1296F2DAC702238A8918E01BA9C3
 */
public class CommentMetricTest extends CommentTestBase {

	/**
	 * Tests how many comments were counted for each comment category. Asserts
	 * that the number of characters counted in the given file under the given
	 * key matches the expected number of characters.
	 */
	public void testCommentDistribution() throws Exception {
		assertMetricAnalysisValue(650,
				CommentMetricAnalysis.KEY_COPYRIGHT_COUNT);
		assertMetricAnalysisValue(85, CommentMetricAnalysis.KEY_HEADER_COUNT);
		assertMetricAnalysisValue(47, CommentMetricAnalysis.KEY_INTERFACE_COUNT);
		assertMetricAnalysisValue(21, CommentMetricAnalysis.KEY_INLINE_COUNT);
		assertMetricAnalysisValue(35, CommentMetricAnalysis.KEY_TASK_COUNT);
		assertMetricAnalysisValue(273,
				CommentMetricAnalysis.KEY_COMMENTED_OUT_CODE_COUNT);
		assertMetricAnalysisValue(23, CommentMetricAnalysis.KEY_SECTION_COUNT);
	}

	/** Runs the comment metric analysis and checks for an expected value. */
	private void assertMetricAnalysisValue(int expectedValue, String key)
			throws Exception {
		assertKeyValue(CommentMetricAnalysis.class,
				"CommentClassification.java", expectedValue, key);
	}
}
