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

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.analysis.AstPositionCalculator.EAstPosition;
import org.conqat.lib.commons.enums.EnumUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests for the {@link AstPositionCalculator}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49809 $
 * @ConQAT.Rating GREEN Hash: C23A42763982DA87F416164EC9314567
 */
public class AstPositionCalculatorTest extends CommentTestBase {

	/**
	 * Tests the calculation of the ast location.
	 */
	public void testASTLocation() throws ConQATException {
		List<Comment> comments = getCommentsInFile("CommentClassification.java");

		List<ShallowEntity> entities = getEntities(comments);

		List<EAstPosition> expectedPositions = Arrays.asList(
				EAstPosition.HEADER, EAstPosition.HEADER,
				EAstPosition.INTERFACE, EAstPosition.INTERFACE,
				EAstPosition.INTERFACE, EAstPosition.INLINE,
				EAstPosition.INTERFACE, EAstPosition.INLINE);

		for (int i = 0; i < expectedPositions.size(); ++i) {
			assertAstPosition(comments.get(i), entities,
					expectedPositions.get(i));
		}
	}

	/**
	 * Asserts that the calculated ast location for the given comment matches
	 * the expected value.
	 */
	private void assertAstPosition(Comment comment,
			List<ShallowEntity> entities, EAstPosition expectedAstPosition) {
		EAstPosition astPosition = AstPositionCalculator.getAstPosition(
				comment.getToken(), entities);
		assertEquals("Mismatch in position for comment starting in line "
				+ (1 + comment.getToken().getLineNumber()),
				expectedAstPosition, astPosition);
	}

	/** Test AST location for all possible comments. */
	public void testASTLocationComplete() throws ConQATException {
		assertAllCommentLocations(getCommentsInFile("ASTLocationTestData.java"));
	}

	/** Tests AST locations for C# properties. */
	public void testCsPropertiesLocation() throws ConQATException {
		assertAllCommentLocations(getCommentsInFile(
				"ast-location-cs-properties.cs", ELanguage.CS));
	}

	/**
	 * Asserts that all comments are recognized at the correct location. For
	 * this, all comments are expected to contain a single label corresponding
	 * to the values of {@link EAstPosition}.
	 */
	private void assertAllCommentLocations(List<Comment> comments)
			throws ConQATException {
		List<ShallowEntity> entities = getEntities(comments);
		for (Comment comment : comments) {
			String text = comment.getText().trim();
			EAstPosition expectedPosition = EnumUtils.valueOfIgnoreCase(
					EAstPosition.class, text);
			assertNotNull("Encountered invalid comment label: " + text,
					expectedPosition);
			assertAstPosition(comment, entities, expectedPosition);
		}
	}

	/**
	 * Returns the shallow parsed entities under the assumption that all given
	 * comments stem from the same element.
	 */
	private static List<ShallowEntity> getEntities(List<Comment> comments)
			throws ConQATException {
		assertNotNull(comments);
		assertTrue("File did not have any comments", !comments.isEmpty());
		List<IToken> tokens = comments.get(0).getTokens();
		return ShallowParserFactory.createParser(tokens.get(0).getLanguage())
				.parseTopLevel(tokens);
	}

}
