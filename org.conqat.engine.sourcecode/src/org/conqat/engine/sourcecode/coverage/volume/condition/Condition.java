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
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.coverage.volume.LineHint;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * A condition inside a statement that consists of multiple tokens.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51131 $
 * @ConQAT.Rating GREEN Hash: 2947FC0DBBE91EC92991AF0DEE2F07BA
 */
public class Condition {

	/** Token classes that identify full words. */
	private static final Set<ETokenClass> WORD_TOKEN_CLASSES = EnumSet.of(
			ETokenClass.IDENTIFIER, ETokenClass.LITERAL, ETokenClass.KEYWORD);

	/** The condition's tokens. */
	private final List<IToken> tokens = new ArrayList<>();

	/** Creates a new empty condition. */
	public Condition() {
		this(new ArrayList<IToken>());
	}

	/** Creates a new condition with the given tokens. */
	public Condition(List<IToken> tokens) {
		CCSMPre.isNotNull(tokens);
		this.tokens.addAll(tokens);
	}

	/** Appends the given token to this condition. */
	public void appendToken(IToken token) {
		CCSMPre.isNotNull(token);
		tokens.add(token);
	}

	/** Returns whether the condition's token list is empty. */
	public boolean isEmpty() {
		return tokens.isEmpty();
	}

	/** Returns the condition's tokens. */
	public UnmodifiableList<IToken> getTokens() {
		return CollectionUtils.asUnmodifiable(tokens);
	}

	/** Returns the line number. */
	public int getLineNumber() {
		if (tokens.isEmpty()) {
			return -1;
		}
		return tokens.get(0).getLineNumber() + 1;
	}

	/**
	 * Removes unbalanced parentheses at the beginning and end as well as
	 * balanced outer pairs of parentheses (as they are not needed).
	 */
	public void cleanupParentheses() {
		int balance = 0;
		for (IToken token : tokens) {
			if (token.getType() == ETokenType.LPAREN) {
				balance += 1;
			} else if (token.getType() == ETokenType.RPAREN) {
				balance -= 1;
			}
		}

		// remove leading "(" and negation
		while (balance > 0
				&& !tokens.isEmpty()
				&& (tokens.get(0).getType() == ETokenType.LPAREN || tokens.get(
						0).getType() == ETokenType.NOT)) {
			if (tokens.get(0).getType() == ETokenType.LPAREN) {
				balance -= 1;
			}
			tokens.remove(0);
		}

		while (balance < 0
				&& !tokens.isEmpty()
				&& CollectionUtils.getLast(tokens).getType() == ETokenType.RPAREN) {
			balance += 1;
			tokens.remove(tokens.size() - 1);
		}

		// Finally, remove unnecessary outer parentheses
		while (balance == 0
				&& !tokens.isEmpty()
				&& tokens.get(0).getType() == ETokenType.LPAREN
				&& CollectionUtils.getLast(tokens).getType() == ETokenType.RPAREN) {
			tokens.remove(0);
			tokens.remove(tokens.size() - 1);
		}
	}

	/**
	 * Return the concatenated text representations of the condition's tokens.
	 */
	public String getText() {
		StringBuilder builder = new StringBuilder();
		boolean previousIsWord = false;
		for (IToken token : tokens) {
			boolean isWord = isWordToken(token);
			if (previousIsWord && isWord) {
				builder.append(StringUtils.SPACE);
			}
			previousIsWord = isWord;
			builder.append(token.getText());
		}
		return builder.toString();
	}

	/**
	 * Returns whether a token is "word-like", i.e. should be handled like a
	 * separate word.
	 */
	private static boolean isWordToken(IToken token) {
		ETokenClass tokenClass = token.getType().getTokenClass();
		return WORD_TOKEN_CLASSES.contains(tokenClass)
				|| (tokenClass == ETokenClass.OPERATOR && Character
						.isAlphabetic(token.getText().charAt(0)));
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		if (tokens.isEmpty()) {
			return StringUtils.EMPTY_STRING;
		}
		return "[" + getText() + "] " + "(" + getLineNumber() + ")";
	}

	/** Creates a line hint from this condition with a custom suffix. */
	public LineHint toLineHint(String suffix) {
		return new LineHint(getText() + " (line " + getLineNumber() + ") - "
				+ suffix, getLineNumber());
	}
}
