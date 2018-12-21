/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenStreamParser.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Allows parsing a token stream from left to right. Does not support
 * backtracking, i.e. it is only possible to advance the parser forward, not
 * backward. This makes it easy to ensure that parsing algorithms have linear
 * time complexity.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: D46E9AC458C0A11E2C1C597ED19F8F4B
 */
public class TokenStreamParser {

	/** The token stream. */
	private final List<IToken> tokens;

	/** The current position of the parser. */
	private int currentToken = 0;

	/** Constructor. */
	public TokenStreamParser(List<IToken> tokens,
			EnumSet<ETokenType> filteredTokens) {
		this.tokens = filter(tokens, filteredTokens);
	}

	/**
	 * Returns a view of the given token list that does not contain any tokens
	 * of the given types.
	 */
	private List<IToken> filter(List<IToken> tokens,
			EnumSet<ETokenType> filteredTokens) {
		List<IToken> filtered = new ArrayList<IToken>();
		for (IToken token : tokens) {
			if (!filteredTokens.contains(token.getType())) {
				filtered.add(token);
			}
		}
		return filtered;
	}

	/** Constructor. */
	public TokenStreamParser(List<IToken> tokens) {
		this.tokens = tokens;
	}

	/**
	 * Consumes all sequential tokens of the given types and returns their
	 * texts.
	 */
	public List<String> consumeAnyOf(EnumSet<ETokenType> types) {
		List<String> texts = new ArrayList<String>();
		while (types.contains(currentType())) {
			texts.add(currentText());
			currentToken++;
		}
		return texts;
	}

	/**
	 * Consumes one token of the given type and returns its text or
	 * <code>null</code> if the current token is not of any of those types. This
	 * method only advances the parser if the current token has one of the given
	 * types.
	 */
	public String consumeOneOrZeroOf(EnumSet<ETokenType> types) {
		if (types.contains(currentType())) {
			String text = currentText();
			currentToken++;
			return text;
		}
		return null;
	}

	/**
	 * Consumes all sequential tokens that are not of the given types and
	 * returns their texts.
	 */
	public List<String> consumeAnyExcept(EnumSet<ETokenType> types) {
		List<String> texts = new ArrayList<String>();
		while (!types.contains(currentType())) {
			if (currentType() == null) {
				break;
			}
			texts.add(currentText());
			currentToken++;
		}
		return texts;
	}

	/**
	 * Skips balanced braces as long as they only contain the allowed inner
	 * types. Assumes that the current token is of the opening type.
	 * 
	 * Skipping may be interrupted prematurely if unexpected tokens are
	 * encountered. In this case, the parser is left pointing to the bad token.
	 * 
	 * @return <code>true</code> if the skipping succeeded and
	 *         <code>false</code> if there were invalid tokens inside the
	 *         balanced braces or the braces were not balanced.
	 */
	public boolean skipBalanced(ETokenType openingType, ETokenType closingType,
			EnumSet<ETokenType> allowedInnerTypes) {
		consumeOneOrZeroOf(EnumSet.of(openingType));
		int openCount = 1;
		while (openCount > 0) {
			consumeAnyOf(allowedInnerTypes);
			if (consumeOneOrZeroOf(EnumSet.of(openingType)) != null) {
				openCount++;
			} else if (consumeOneOrZeroOf(EnumSet.of(closingType)) != null) {
				openCount--;
			} else {
				return false;
			}
		}
		return true;
	}

	/**
	 * Consumes tokens as long as they alternate between the two type sets.
	 * 
	 */
	public List<String> consumeAlternating(EnumSet<ETokenType> typeSet1,
			EnumSet<ETokenType> typeSet2) {
		List<String> list = new ArrayList<String>();
		while (true) {
			String token1 = consumeOneOrZeroOf(typeSet1);
			if (token1 == null) {
				break;
			}
			list.add(token1);
			String token2 = consumeOneOrZeroOf(typeSet2);
			if (token2 == null) {
				break;
			}
			list.add(token2);
		}
		return list;
	}

	/**
	 * Returns <code>true</code> if the current token has any of the given
	 * types.
	 */
	public boolean isAnyOf(EnumSet<ETokenType> types) {
		return types.contains(currentType());
	}

	/** Returns <code>true</code> if all tokens have been consumed. */
	public boolean isDone() {
		return currentToken >= tokens.size();
	}

	/**
	 * Returns the text of the current token or <code>null</code> if all tokens
	 * have been consumed.
	 */
	public String currentText() {
		if (isDone()) {
			return null;
		}
		return tokens.get(currentToken).getText();
	}

	/**
	 * Returns the type of the current token or <code>null</code> if all tokens
	 * have been consumed.
	 */
	public ETokenType currentType() {
		if (isDone()) {
			return null;
		}
		return tokens.get(currentToken).getType();
	}

	/**
	 * Returns the number of tokens consumed so far.
	 */
	public int getConsumedTokenCount() {
		return currentToken;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "TokenStreamParser "
				+ tokens.subList(currentToken, tokens.size());
	}

}
