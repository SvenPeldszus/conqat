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
 * Test for counting inline comments. Tests the class
 * {@link InlineCommentCounter}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49784 $
 * @ConQAT.Rating GREEN Hash: 296DDB48C74735AE1235AD616716179A
 */
public class InlineCommentCounterTest extends CommentTestBase {

	/** Tests counting inline comments. */
	public void testCounts() throws Exception {
		assertKeyValue(InlineCommentCounter.class,
				"CommentFindings.java", 4,
				InlineCommentCounter.KEY_NUM_INLINE_COMMENTS);
	}
}