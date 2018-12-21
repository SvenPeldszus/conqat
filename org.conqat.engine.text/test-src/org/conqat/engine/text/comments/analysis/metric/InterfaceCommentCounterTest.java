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
 * Test for counting interface comments. Tests the classes
 * {@link InterfaceCommentCounter}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49784 $
 * @ConQAT.Rating GREEN Hash: 516FB4EE3C0B1F55B10CEEAB47D8AE08
 */
public class InterfaceCommentCounterTest extends CommentTestBase {

	/** Tests counting interface comments. */
	public void testCounts() throws Exception {

		assertKeyValue(InterfaceCommentCounter.class,
				"CommentFindings.java", 5,
				InterfaceCommentCounter.KEY_NUM_INTERFACE_COMMENTS);
	}
}