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
package org.conqat.engine.text.comments.analysis;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.text.comments.Comment;

/**
 * Test to extract comments from a file. Test the {@link CommentExtractor}
 * class.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49777 $
 * @ConQAT.Rating GREEN Hash: 1D585D7299C951395F236DC24E03EA37
 */
public class CommentExtractorTest extends CommentTestBase {

	/**
	 * Tests that the correct number of comments is extracted
	 */
	public void testCommentExtraction() throws ConQATException {
		List<Comment> comments = getCommentsInFile("CommentClassification.java");
		assertNotNull(comments);
		assertEquals(8, comments.size());
		assertTrue(comments.get(6).getText().contains("return result;"));

		comments = getCommentsInFile("ASTLocationTestData.java");
		assertNotNull(comments);
		assertEquals(15, comments.size());
		assertEquals("HEADER", comments.get(0).getText());
	}
}
