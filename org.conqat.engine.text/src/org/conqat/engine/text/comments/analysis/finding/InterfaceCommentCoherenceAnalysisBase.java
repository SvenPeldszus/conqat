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
package org.conqat.engine.text.comments.analysis.finding;

import java.util.List;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.ECommentCategory;
import org.conqat.engine.text.comments.classification.CoherenceUtils;
import org.conqat.engine.text.comments.utils.CommentUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Processor to analyze the coherence between interface comment and method name.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51561 $
 * @ConQAT.Rating GREEN Hash: B6AA2F3B68D9FCA2BE7F1D6842C368A7
 */
public abstract class InterfaceCommentCoherenceAnalysisBase extends
		CommentFindingAnalysisBase {

	/** Pattern matching a non-identifier character. */
	private static final Pattern NON_IDENTIFIER_PATTERN = Pattern
			.compile("[^a-zA-Z0-9_]");

	/** Pattern matching Javadoc "@link" tags. */
	private static final Pattern JAVADOC_AT_LINK_TAG_PATTERN = Pattern
			.compile("\\{@link #?(.*?)\\}");

	/** Constructor. */
	protected InterfaceCommentCoherenceAnalysisBase(String findingGroupKey) {
		super(findingGroupKey);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Analyzes interface comments by calling the abstract method to calculate
	 * the coherence between comment and method name.
	 */
	@Override
	protected void analyzeComment(IElement element, Comment comment,
			ECommentCategory category) throws ConQATException {
		if (category != ECommentCategory.INTERFACE) {
			return;
		}
		if (!CommentUtils.isDefaultComment(comment.getText())
				&& !CommentUtils.hasOnlyJavaDoc(comment.getText())
				&& hasFollowupMethodOrIdentifier(comment)) {
			analyzeCoherence(comment);
		}
	}

	/**
	 * Checks whether the comment is in front of a method or attribute and not
	 * just somewhere at the end of the class
	 */
	private boolean hasFollowupMethodOrIdentifier(Comment comment) {
		List<IToken> nextTokens = comment.getNextTokens(
				comment.getTokenIndex(), 1);
		if (nextTokens.isEmpty()) {
			// No more tokens, can't be interface comment
			return false;
		}
		IToken nextToken = nextTokens.get(0);
		if (nextToken.getType().equals(ETokenType.IDENTIFIER)) {
			// Comment before identifier, that's ok
			return true;
		}
		// Last resort, see if there is a method after the comment at all
		return !StringUtils.isEmpty(comment.getMethodFinder()
				.getNextDefinition(comment.getTokenIndex(), false));
	}

	/**
	 * Abstract method to analyze the coherence between method name and comment.
	 */
	protected abstract void analyzeCoherence(Comment comment)
			throws ConQATException;

	/**
	 * Returns the coherence coefficient, which indicates how many words in the
	 * comment correspond to a word in the method name relative to all words in
	 * the comment. Returns -1 if no next definition is found or the next
	 * definition refers to some construct without identifier (operator
	 * overloading, etc.).
	 */
	protected double getCoherenceCoefficient(Comment comment) {
		String commentText = comment.getText();

		String nextDefinition = comment.getMethodFinder().getNextDefinition(
				comment.getTokenIndex());
		if (StringUtils.isEmpty(nextDefinition)) {
			return -1;
		}

		// operator overloading
		if (NON_IDENTIFIER_PATTERN.matcher(nextDefinition).find()
				|| "this".equals(nextDefinition)) {
			return -1;
		}

		double numberOfCorrespondingWords = 0;
		if (nextDefinition.length() == 1
				&& StringUtils.containsIgnoreCase(commentText, nextDefinition)) {
			numberOfCorrespondingWords = 1;
		} else {
			numberOfCorrespondingWords = CoherenceUtils
					.getNumCorrespondingWords(nextDefinition, commentText,
							comment.getLanguage());

		}

		double numWordsInComment = 0;
		String[] parts = commentText.split("\\s+");
		for (String part : parts) {
			// ignore dots and other single characters
			if (part.length() > 1) {
				numWordsInComment++;
			}
		}

		return numberOfCorrespondingWords / numWordsInComment;

	}

	/**
	 * Returns a copy of the given comment that contains the comment content
	 * until the first @.
	 * 
	 * If includingReturn is true, then a potential @return string is included.
	 */
	protected Comment getCommentHeadline(Comment comment,
			boolean includingReturn) {
		String text = comment.getText();
		text = JAVADOC_AT_LINK_TAG_PATTERN.matcher(text).replaceAll("$1");
		String commentStringAdapted = StringUtils.getFirstParts(text, 1, '@');
		String[] returnSplit = text.split("@return");

		if (includingReturn && returnSplit.length > 1) {
			// as the return statement might be followed by further @throw etc.
			// declaration, cut them off.
			commentStringAdapted += StringUtils.getFirstParts(returnSplit[1],
					1, '@');
		}

		return new Comment(commentStringAdapted, comment.getTokenIndex(),
				comment.getAstPosition(), comment.getElement(),
				comment.getTokens(), comment.getMethodFinder());
	}
}
