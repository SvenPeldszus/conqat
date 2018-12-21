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
package org.conqat.engine.text.comments.analysis.metric;

import java.util.List;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.engine.text.comments.analysis.CommentClassificationAnalysisBase;
import org.conqat.lib.commons.collections.CounterSet;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49769 $
 * @ConQAT.Rating GREEN Hash: 90CB4422FF33CFA33C6A27626EF0E4DE
 */
@AConQATProcessor(description = "Analyzes the comments by calculating the overall comment ratio"
		+ "and the distribution over copyright, header, interface, inline, section, and task"
		+ "comments as well as commented out code using machine learning.")
public class CommentMetricAnalysis extends CommentClassificationAnalysisBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to a comment.", type = "java.lang.Integer")
	public static final String KEY_COMMENT_COUNT = "comment";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts the complete source code in characters (including comments).", type = "java.lang.Integer")
	public static final String KEY_CODE_COUNT = "code";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to a copyright comment.", type = "java.lang.Integer")
	public static final String KEY_COPYRIGHT_COUNT = "copyright";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to a header comment.", type = "java.lang.Integer")
	public static final String KEY_HEADER_COUNT = "header";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to an interface comment.", type = "java.lang.Integer")
	public static final String KEY_INTERFACE_COUNT = "interface";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to an inline comment.", type = "java.lang.Integer")
	public static final String KEY_INLINE_COUNT = "inline";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to a section comment.", type = "java.lang.Integer")
	public static final String KEY_SECTION_COUNT = "section";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to a task comment.", type = "java.lang.Integer")
	public static final String KEY_TASK_COUNT = "task";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Counts characters belonging to commented out code.", type = "java.lang.Integer")
	public static final String KEY_COMMENTED_OUT_CODE_COUNT = "commented-out-code";

	/**
	 * Denotes the distribution over the different comment categories of the
	 * current file. The values of the map denote the character count for each
	 * comment category.
	 */
	private CounterSet<ECommentCategory> commentDistribution;

	/** Counts all comment characters in the underlying element. */
	private int commentCharacterCount;

	/**
	 * {@inheritDoc} Sets all metrics to zero except of the code count which
	 * only needs to be calculated once per underlying element.
	 * */
	@Override
	protected void setUpElementAnalysis() {
		commentDistribution = new CounterSet<ECommentCategory>();
		commentCharacterCount = 0;
	}

	/**
	 * Returns the total count of characters within a token list.
	 */
	private int getCodeCharacterCount(List<IToken> tokens) {
		int totalCharacterCount = 0;
		for (IToken token : tokens) {
			totalCharacterCount += StringUtils
					.removeWhitespace(token.getText()).length();
		}
		return totalCharacterCount;
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComments(List<Comment> comments,
			ITokenElement element, List<IToken> tokens) throws ConQATException {
		// we want elements with unsupported languages to be skipped silently,
		// as we can also generate useful output for these elements in
		// completeElementAnalysis()
		if (!isSupportedLanguage(element.getLanguage())) {
			return;
		}

		super.analyzeComments(comments, element, tokens);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) {
		commentCharacterCount += getCommentLength(comment.getText());
		commentDistribution.inc(category, getCommentLength(comment.getText()));
	}

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] { KEY_COMMENT_COUNT, KEY_CODE_COUNT,
				KEY_COPYRIGHT_COUNT, KEY_HEADER_COUNT, KEY_INTERFACE_COUNT,
				KEY_INLINE_COUNT, KEY_TASK_COUNT, KEY_COMMENTED_OUT_CODE_COUNT,
				KEY_SECTION_COUNT };
	}

	/**
	 * Returns the length of a comment in characters after removing white
	 * spaces.
	 */
	private static int getCommentLength(String comment) {
		return StringUtils.removeWhitespace(comment).length();
	}

	/** {@inheritDoc} */
	@Override
	protected void completeElementAnalysis(List<IToken> tokens,
			ITokenElement element) {
		int codeCharacterCount = 0;
		if (!isSupportedLanguage(element.getLanguage())) {
			try {
				codeCharacterCount = element.getTextContent().length();
				commentCharacterCount = calculateCommentCountLanguageIndependent(tokens);
			} catch (ConQATException e) {
				getLogger().error(e.getMessage());
			}
		} else {
			codeCharacterCount = getCodeCharacterCount(tokens);
		}

		element.setValue(KEY_COMMENT_COUNT, commentCharacterCount);
		element.setValue(KEY_CODE_COUNT, codeCharacterCount);

		element.setValue(KEY_COPYRIGHT_COUNT,
				commentDistribution.getValue(ECommentCategory.COPYRIGHT));
		element.setValue(KEY_HEADER_COUNT,
				commentDistribution.getValue(ECommentCategory.HEADER));
		element.setValue(KEY_INTERFACE_COUNT,
				commentDistribution.getValue(ECommentCategory.INTERFACE));
		element.setValue(KEY_INLINE_COUNT,
				commentDistribution.getValue(ECommentCategory.INLINE));
		element.setValue(KEY_SECTION_COUNT,
				commentDistribution.getValue(ECommentCategory.SECTION));
		element.setValue(KEY_TASK_COUNT,
				commentDistribution.getValue(ECommentCategory.TASK));
		element.setValue(KEY_COMMENTED_OUT_CODE_COUNT,
				commentDistribution.getValue(ECommentCategory.CODE));

	}

	/**
	 * Calculates the number of characters in comments language independent and
	 * independent from included tokens.
	 */
	private static int calculateCommentCountLanguageIndependent(
			List<IToken> tokens) {
		int commentCount = 0;
		for (IToken token : tokens) {
			if (token.getType().getTokenClass() == ETokenClass.COMMENT) {
				commentCount += getCommentLength(token.getText());
			}
		}
		return commentCount;
	}
}
