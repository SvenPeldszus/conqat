/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenStream.java 51549 2015-01-19 09:51:34Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import java.util.List;

import org.conqat.lib.scanner.IToken;

/**
 * Wraps a list of tokens for easier parsing.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51549 $
 * @ConQAT.Rating YELLOW Hash: 86F42A470258AE17D3691CE67F8EB0B7
 */
/* package */class TokenStream {

	/** The parsed tokens. */
	private final List<IToken> tokens;

	/** The current position in the stream. */
	private int position;

	/** Constructor. */
	public TokenStream(List<IToken> tokens, int position) {
		this.tokens = tokens;
		this.position = position;
	}

	/**
	 * Sets the position of the stream to the given one.
	 */
	public void setPosition(int position) {
		this.position = position;
	}

	/**
	 * Returns <code>true</code> if the steam is positioned on the first token.
	 */
	public boolean isAtBeginning() {
		return position == 0;
	}

	/**
	 * Returns true if the stream is positioned past the last token.
	 */
	public boolean isExhausted() {
		return position >= tokens.size();
	}

	/**
	 * Returns a view of the tokens in between the given position (inclusive)
	 * and the current position (exclusive).
	 */
	public List<IToken> getTokensSince(int startPosition) {
		return tokens.subList(startPosition, position);
	}

	/**
	 * Advances the stream by one token.
	 * 
	 * @return The token the stream was positioned on before advancing or
	 *         <code>null</code> if the stream was already exhausted at that
	 *         time.
	 */
	public IToken next() {
		if (position >= tokens.size()) {
			return null;
		}
		position += 1;
		return getAtPosition(position - 1);
	}

	/**
	 * Returns the token at the given position or <code>null</code> if the
	 * position is out of bounds.
	 */
	private IToken getAtPosition(int position) {
		if (position >= tokens.size() || position < 0) {
			return null;
		}
		return tokens.get(position);
	}

	/**
	 * Returns the token before the current one or <code>null</code> if the
	 * stream is at the beginning.
	 */
	public IToken peekBackward() {
		return getAtPosition(position - 1);
	}

	/**
	 * Returns the current token or <code>null</code> if the stream is
	 * exhausted.
	 */
	public IToken peekCurrent() {
		return getAtPosition(position);
	}

	/**
	 * Returns the current position in the stream. May be used to reset the
	 * stream to that position at a later time using {@link #setPosition(int)}.
	 */
	public int getPosition() {
		return position;
	}

	/**
	 * Moves the stream's position back by one token and returns the token at
	 * that position or <code>null</code> if the stream was already at the
	 * beginning.
	 */
	public IToken moveBack() {
		if (position <= 0) {
			return null;
		}
		position -= 1;
		return peekCurrent();
	}
}
