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
package org.conqat.engine.text.comments;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.text.comments.analysis.AstPositionCalculator.EAstPosition;
import org.conqat.engine.text.comments.classification.MethodFinder;
import org.conqat.engine.text.comments.utils.CommentTaggingUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Wrapper for a comment within source code. Includes context information about
 * the comment used in machine learning for comment classification.
 * 
 * @author $Author: steidl$
 * @version $Rev: 49754 $
 * @ConQAT.Rating GREEN Hash: EA84A1ECDD6B5B38A8F6C4F138012D07
 */
public class Comment {

	/** The comment text without any comment delimiters. */
	private final String text;

	/** Position of the comment token in the underlying {@link #tokens}. */
	private final int tokenIndex;

	/** The logical position in the AST. */
	private final EAstPosition astPosition;

	/** The underlying element. */
	private final ITokenElement element;

	/** The underlying tokens. */
	private final List<IToken> tokens;

	/** Context class that allows fast lookup of methods next to the comment. */
	private final MethodFinder methodFinder;

	/** Constructor. */
	public Comment(String comment, int tokenIndex, EAstPosition astPosition,
			ITokenElement element, List<IToken> tokens,
			MethodFinder methodFinder) {
		this.tokenIndex = tokenIndex;
		this.astPosition = astPosition;
		this.element = element;
		this.tokens = tokens;
		this.methodFinder = methodFinder;

		this.text = tokens.get(tokenIndex).getLanguage()
				.getCommentContent(comment);
	}

	/** Returns {@link #methodFinder}. */
	public MethodFinder getMethodFinder() {
		return methodFinder;
	}

	/** Returns {@link #astPosition}. */
	public EAstPosition getAstPosition() {
		return astPosition;
	}

	/**
	 * Returns {@link #tokenIndex}.
	 */
	public int getTokenIndex() {
		return tokenIndex;
	}

	/** Returns {@link #text}. */
	public String getText() {
		return text;
	}

	/**
	 * Returns the raw text of the comment. This is extracted from the token and
	 * also potentially contains tags (see {@link CommentTaggingUtils}).
	 */
	public String getRawCommentText() {
		return getToken().getText();
	}

	/**
	 * Returns the next tokens in the underlying token list, starting from token
	 * at position start. If there are not enough tokens in the list, less
	 * tokens will be returned.
	 * 
	 * @param count
	 *            the maximal number of tokens to be returned
	 */
	public List<IToken> getNextTokens(int start, int count) {
		List<IToken> nextTokens = new ArrayList<IToken>();
		int end = Math.min(tokens.size(), start + count + 1);
		for (int i = start + 1; i < end; i++) {
			nextTokens.add(tokens.get(i));
		}
		return nextTokens;
	}

	/** Returns the tokens of the file which contains the comment. */
	public UnmodifiableList<IToken> getTokens() {
		return CollectionUtils.asUnmodifiable(tokens);
	}

	/** Returns the element which contains the comments. */
	public ITokenElement getElement() {
		return element;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return text;
	}

	/** Returns the token for this comment. */
	public IToken getToken() {
		return tokens.get(tokenIndex);
	}

	/** Returns the language of the comment. */
	public ELanguage getLanguage() {
		return getToken().getLanguage();
	}
}
