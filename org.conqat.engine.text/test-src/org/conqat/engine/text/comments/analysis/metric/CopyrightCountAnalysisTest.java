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
 * Tests the {@link CopyrightCountAnalysis}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49784 $
 * @ConQAT.Rating GREEN Hash: 043DA148815056701A4D59D3BAED3AA5
 */
public class CopyrightCountAnalysisTest extends CommentTestBase {

	/**
	 * Test detection of copyrights. Asserts number of copyrights found is as
	 * expected.
	 */
	public void testCopyright() throws Exception {
		assertKeyValue(CopyrightCountAnalysis.class,
				"CompletelyCommentedClass.java", 1,
				CopyrightCountAnalysis.KEY_NUM_COPYRIGHTS);
		assertKeyValue(CopyrightCountAnalysis.class,
				"HalfCommentedClass.java", 1,
				CopyrightCountAnalysis.KEY_NUM_COPYRIGHTS);
		assertKeyValue(CopyrightCountAnalysis.class,
				"ClassWithNoComments.java", 0,
				CopyrightCountAnalysis.KEY_NUM_COPYRIGHTS);
	}
}