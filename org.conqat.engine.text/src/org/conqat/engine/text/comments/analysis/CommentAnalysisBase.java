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

package org.conqat.engine.text.comments.analysis;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.analysis.TokenAnalyzerBase;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.utils.CommentTaggingUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for a processor that analyzes code comments. This class does the
 * analysis to provide a list of comments for each file. A comment is thereby
 * annotated with all required information for machine learning or machine
 * classification.
 * 
 * This only analyses elements for languages which are supported by the shallow
 * parser. Elements of other languages are analyzed with an empty list of
 * comments and so basically ignored.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49757 $
 * @ConQAT.Rating GREEN Hash: 9121F1B827CFAAB95ADF822F2AA49F17
 */
public abstract class CommentAnalysisBase extends TokenAnalyzerBase {

	/**
	 * Pattern to detect comments which are package visibility comments
	 * (specific to ConQAT development).
	 */
	private static final Pattern PACKAGE_VISIBILITY_COMMENT_PATTERN = Pattern
			.compile("/\\*\\s?package\\s?\\*/");

	/** If this is not empty, only tokens included in this set are respected. */
	private final Set<ETokenType> includedTokenTypes = EnumSet
			.noneOf(ETokenType.class);

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "comment-type", description = "This parameter allows to "
			+ "set specific token types. If this is set, only comments of the specified type are respected. "
			+ "If not set, all comment tokens are respected. This parameter raises an error if a token type "
			+ "is provided that is not a comment.")
	public void addCommentType(
			@AConQATAttribute(name = "value", description = "comment token type") ETokenType tokenType)
			throws ConQATException {
		if (tokenType.getTokenClass() != ETokenClass.COMMENT) {
			throw new ConQATException("Token type " + tokenType
					+ " is not a comment!");
		}
		includedTokenTypes.add(tokenType);
	}

	/** {@inheritDoc} */
	@SuppressWarnings("unused")
	@Override
	protected void analyzeTokens(List<IToken> tokens, ITokenElement element)
			throws ConQATException {
		setUpElementAnalysis();

		try {
			List<Comment> comments = extractComments(tokens, element);
			analyzeComments(comments, element, tokens);
			completeElementAnalysis(tokens, element);
		} catch (ConQATException e) {
			getLogger().error(
					"Error in extracting comments from element "
							+ element.getUniformPath(), e);
		}
	}

	/**
	 * Extracts and returns the list of comments for an element. For languages
	 * without a shallow parser, this returns the empty list.
	 */
	private List<Comment> extractComments(List<IToken> tokens,
			ITokenElement element) throws ConQATException {
		if (!ShallowParserFactory.supportsLanguage(element.getLanguage())) {
			return new ArrayList<>();
		}

		List<IToken> unifiedCommentTokens = unifyMultipleSingleLineComments(tokens);

		if (element.getLanguage() == ELanguage.JAVA) {
			unifiedCommentTokens = removePackageVisibilityComments(unifiedCommentTokens);
		}

		return CommentExtractor.extractComments(unifiedCommentTokens, element,
				includedTokenTypes, isRemoveCommentTags());
	}

	/**
	 * Returns whether comment tags (see {@link CommentTaggingUtils}) should be
	 * removed. Default returns false.
	 */
	protected boolean isRemoveCommentTags() {
		return false;
	}

	/** Template method for analyzing the comments of an element. */
	protected abstract void analyzeComments(List<Comment> comments,
			ITokenElement element, List<IToken> tokens) throws ConQATException;

	/**
	 * Set up method for analyzing a new element. This can be used to reset
	 * counters.
	 */
	protected void setUpElementAnalysis() {
		// Empty default implementation.
	}

	/** Finish method after analyzing an element with its tokens */
	@SuppressWarnings("unused")
	protected void completeElementAnalysis(List<IToken> tokens,
			ITokenElement element) {
		// Empty default implementation.
	}

	/**
	 * Returns a token list which has consecutive single line comments merged to
	 * one comment.
	 */
	public static List<IToken> unifyMultipleSingleLineComments(
			List<IToken> tokens) {
		List<IToken> result = new ArrayList<>();
		for (int i = 0; i < tokens.size(); i++) {
			IToken token = tokens.get(i);
			if (!isSingleLineCommentToken(token)) {
				result.add(token);
				continue;
			}

			StringBuilder commentBuilder = new StringBuilder(token.getText());
			int j = i + 1;
			for (; j < tokens.size(); j++) {
				if (isSingleLineCommentToken(tokens.get(j))
						&& diffLineNumber(token, tokens.get(j)) == (j - i)) {
					commentBuilder.append(tokens.get(j).getText());
				} else {
					break;
				}
			}

			if (commentBuilder.length() > token.getText().length()) {
				result.add(token.newToken(token.getType(), token.getOffset(),
						token.getLineNumber(), commentBuilder.toString(),
						token.getOriginId()));
				i = j - 1;
			} else {
				result.add(token);
			}
		}
		return result;
	}

	/**
	 * Decides whether the given comment is a single line comment and hence
	 * should be merged.
	 */
	private static boolean isSingleLineCommentToken(IToken token) {
		if (token.getType() == ETokenType.END_OF_LINE_COMMENT) {
			return true;
		}

		// in C# doc comments are single line comments as well
		if (token.getLanguage() == ELanguage.CS) {
			return token.getType() == ETokenType.DOCUMENTATION_COMMENT;
		}

		// in C++ there are doxygen single line and multiline comments, so we
		// have to check the text content
		if (token.getLanguage() == ELanguage.CPP) {
			return token.getText().startsWith("//");
		}

		return false;
	}

	/**
	 * Removes all comments that are package visibility comments and returns the
	 * new list.
	 */
	private List<IToken> removePackageVisibilityComments(List<IToken> tokens) {
		List<IToken> result = new ArrayList<>();
		for (IToken token : tokens) {
			if (token.getType() == ETokenType.TRADITIONAL_COMMENT
					&& PACKAGE_VISIBILITY_COMMENT_PATTERN.matcher(
							token.getText()).matches()) {
				continue;
			}

			result.add(token);
		}
		return result;
	}

	/**
	 * Returns the absolute value of the line number difference between two
	 * tokens.
	 */
	private static int diffLineNumber(IToken token1, IToken token2) {
		return Math.abs(token1.getLineNumber() - token2.getLineNumber());
	}

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] {};
	}
}
