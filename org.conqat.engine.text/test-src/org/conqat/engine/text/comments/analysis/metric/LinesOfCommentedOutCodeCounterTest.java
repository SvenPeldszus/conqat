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
 * Test for counting comments. Tests the class
 * {@link LinesOfCommentedOutCodeCounter}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49784 $
 * @ConQAT.Rating GREEN Hash: 3C919BF5983E16A9566B157C32F92C55
 */
public class LinesOfCommentedOutCodeCounterTest extends CommentTestBase {

	/** Tests counting lines of commented out code. */
	public void testCounts() throws Exception {
		assertKeyValue(LinesOfCommentedOutCodeCounter.class,
				"CommentFindings.java", 7,
				LinesOfCommentedOutCodeCounter.KEY_COMMENTED_OUT_CODE_LOC);
	}
}