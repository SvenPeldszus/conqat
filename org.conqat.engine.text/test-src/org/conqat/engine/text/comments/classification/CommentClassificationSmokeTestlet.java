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
package org.conqat.engine.text.comments.classification;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.engine.text.comments.analysis.CommentAnalysisBase;
import org.conqat.engine.text.comments.analysis.CommentClassificationAnalysisBase;
import org.conqat.engine.text.comments.analysis.CommentExtractor;
import org.conqat.engine.text.comments.analysis.ICommentClassifier;
import org.conqat.engine.text.comments.utils.CommentTaggingUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.test.TestletBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.junit.Ignore;

/**
 * Single test for the {@link CommentClassificationSmokeTest}.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49812 $
 * @ConQAT.Rating GREEN Hash: F48F1004D891570F3100786F9B2D2B75
 */
@Ignore
public class CommentClassificationSmokeTestlet extends TestletBase {

	/** File under test. */
	private final File file;

	/** Constructor. */
	public CommentClassificationSmokeTestlet(File file) {
		this.file = file;
	}

	/** {@inheritDoc} */
	@Override
	public void test() throws Exception {
		ELanguage language = ELanguage.fromFile(file);
		List<Comment> comments = extractComments(language);

		ICommentClassifier classifier = CommentClassificationAnalysisBase
				.getCommentClassifier(language);
		assertNotNull("Unsupported language: " + language, classifier);

		StringBuilder expectedBuilder = new StringBuilder();
		StringBuilder actualBuilder = new StringBuilder();

		int count = 0;
		for (Comment comment : comments) {
			ECommentCategory category = CommentTaggingUtils
					.getAnnotatedCategory(comment);
			if (category == null) {
				continue;
			}
			count += 1;

			expectedBuilder.append("Comment " + count + " (classification: "
					+ category + "):\n");
			ECommentCategory actualCategory = classifier
					.getClassification(comment);
			actualBuilder.append("Comment " + count + " (classification: "
					+ actualCategory + "):\n");

			expectedBuilder.append(comment.getRawCommentText());
			actualBuilder.append(comment.getRawCommentText());

			expectedBuilder.append("\n\n");
			actualBuilder.append("\n\n");
		}

		assertEquals(
				StringUtils.normalizeLineBreaks(expectedBuilder.toString()),
				StringUtils.normalizeLineBreaks(actualBuilder.toString()));
	}

	/** Returns the comments contained in {@link #file}. */
	private List<Comment> extractComments(ELanguage language)
			throws FileNotFoundException, IOException, ConQATException {
		ITokenElement element = TokenTestCaseBase.createTokenElementFromFile(language, file);
		TokenTestCaseBase.checkForIncompleteEntities(element);

		List<IToken> tokens = CommentAnalysisBase
				.unifyMultipleSingleLineComments(element
						.getTokens(new LoggerMock()));
		return CommentExtractor.extractComments(tokens, element,
				CollectionUtils.<ETokenType> emptySet(), true);
	}

	/** Name of the test case is the name of the test file. */
	@Override
	public String getName() {
		return file.getName();
	}
}
