/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenPattern.java 51547 2015-01-19 09:49:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.dataflow.utils.tokens.ITokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStream;
import org.conqat.engine.sourcecode.pattern.TokenTypePattern;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * A pattern that can be applied like a regular expression to a token stream.
 * 
 * This class is not fully redundant to {@link TokenTypePattern} for two
 * reasons:
 * <ul>
 * <li>It allows matching nested structures, e.g. parentheses, with
 * {@link #skipNested(Object, Object, boolean)}
 * <li>It allows named capture groups, something which is not currently possible
 * with Java 7: http://bugs.java.com/bugdatabase/view_bug.do?bug_id=8013252
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51547 $
 * @ConQAT.Rating YELLOW Hash: FBE2801D4DC1B18ECE99CFD25B58A516
 */
public class TokenPattern implements ITokenPattern {

	/** The parts of the pattern that are matched in sequence. */
	private final PairList<ITokenSubpatternMatcher, Integer> submatchers = new PairList<ITokenSubpatternMatcher, Integer>();

	/**
	 * Matches the pattern at all positions in the given token stream and
	 * returns the found matches.
	 */
	public List<TokenPatternMatch> match(List<IToken> tokens) {
		List<TokenPatternMatch> matches = new ArrayList<TokenPatternMatch>();
		for (int i = 0; i < tokens.size(); i++) {
			TokenPatternMatch match = new TokenPatternMatch(tokens);
			TokenStream stream = new TokenStream(tokens, i);
			if (matches(stream, match)) {
				matches.add(match);
			}
		}
		return matches;
	}

	/**
	 * Matches the pattern at all positions in the given token stream and
	 * returns the first found match or <code>null</code> if no match happened.
	 */
	public TokenPatternMatch matchFirst(List<IToken> tokens) {
		for (int i = 0; i < tokens.size(); i++) {
			TokenPatternMatch match = new TokenPatternMatch(tokens);
			TokenStream stream = new TokenStream(tokens, i);
			if (matches(stream, match)) {
				return match;
			}
		}
		return null;
	}

	/**
	 * Returns <code>true</code> if the pattern matches at least once in the
	 * given token stream.
	 */
	public boolean matches(List<IToken> tokens) {
		return matchFirst(tokens) != null;
	}

	/**
	 * Matches the pattern once at the beginnig of the given token stream and
	 * records the results in the given match.
	 * 
	 * NOTE: Consider using the convenience methods {@link #match(List)},
	 * {@link #matches(List)} and {@link #matchFirst(List)} instead of this
	 * method.
	 * 
	 * @return <code>true</code> if the pattern matched or <code>false</code> if
	 *         the given match should be discarded.
	 */
	@Override
	public boolean matches(TokenStream stream, TokenPatternMatch match) {
		for (int i = 0; i < submatchers.size(); i++) {
			ITokenSubpatternMatcher matcher = submatchers.getFirst(i);
			int beforeMatch = stream.getPosition();
			if (!matcher.match(stream, match)) {
				return false;
			}

			Integer group = submatchers.getSecond(i);
			if (group != null) {
				match.appendToGroup(group, beforeMatch, stream.getPosition());
			}
		}
		return true;
	}

	/**
	 * Specifies that the last added subpattern should be appended to the group
	 * with the given index.
	 */
	public TokenPattern group(int groupIndex) {
		submatchers.setSecond(submatchers.size() - 1, groupIndex);
		return this;
	}

	/**
	 * Adds a new pattern part that consumes anything until it encounters one of
	 * the given match terms. The stop token matching one of the given terms is
	 * also consumed.
	 * 
	 * A group applied to this pattern will only contain the stop token.
	 * 
	 * @param matchTerms
	 *            a list of match terms which must match in order. These may be
	 *            instances of {@link ETokenType}, {@link ETokenClass}, or sets
	 *            of them, as well as {@link TokenPattern}s.
	 */
	public TokenPattern skipTo(Object... matchTerms) {
		final ITokenPattern[] matchers = convertMatchTerms(matchTerms);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				while (true) {
					if (stream.isExhausted()) {
						return false;
					}

					for (ITokenPattern matcher : matchers) {
						int beforeMatch = stream.getPosition();
						boolean success = matcher.matches(stream,
								createDummyMatch());
						stream.setPosition(beforeMatch);
						if (success) {
							return true;
						}
					}

					stream.next();
				}
			}
		}, null);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				for (ITokenPattern matcher : matchers) {
					TokenPatternMatch subMatch = createDummyMatch();
					int beforeMatch = stream.getPosition();
					if (matcher.matches(stream, subMatch)) {
						match.mergeFrom(subMatch);
						return true;
					}
					stream.setPosition(beforeMatch);
				}
				return false;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that consumes nested structures.
	 * 
	 * @param openTerm
	 *            a match term that identifies the opening braces. May be an
	 *            instance of {@link ETokenType}, {@link ETokenClass}, or a set
	 *            of them, as well as an {@link TokenPattern}.
	 * @param closeTerm
	 *            a match term that identifies the closing braces. May be an
	 *            instance of {@link ETokenType}, {@link ETokenClass}, or a set
	 *            of them, as well as an {@link TokenPattern}.
	 * @param optional
	 *            if this is <code>false</code>, the nested structure must be
	 *            present or the pattern will not match. If this is
	 *            <code>true</code>, this pattern may return an empty match.
	 */
	public TokenPattern skipNested(Object openTerm, Object closeTerm,
			final boolean optional) {
		final ITokenPattern openMatcher = convertMatchTerm(openTerm);
		final ITokenPattern closeMatcher = convertMatchTerm(closeTerm);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int beforeMatch = stream.getPosition();
				TokenPatternMatch subMatch = createDummyMatch();

				if (!openMatcher.matches(stream, subMatch)) {
					stream.setPosition(beforeMatch);
					return optional;
				}

				int level = 1;
				while (level > 0) {
					if (stream.isExhausted()) {
						stream.setPosition(beforeMatch);
						return optional;
					}

					int beforeAlternative = stream.getPosition();
					TokenPatternMatch alternativeSubMatch = createDummyMatch();
					if (closeMatcher.matches(stream, alternativeSubMatch)) {
						subMatch.mergeFrom(alternativeSubMatch);
						level -= 1;
						continue;
					}
					stream.setPosition(beforeAlternative);

					alternativeSubMatch = createDummyMatch();
					if (openMatcher.matches(stream, alternativeSubMatch)) {
						subMatch.mergeFrom(alternativeSubMatch);
						level += 1;
						continue;
					}
					stream.setPosition(beforeAlternative);

					stream.next();
				}

				match.mergeFrom(subMatch);
				return true;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches the given terms zero or more times
	 * in the given order.
	 * 
	 * @param matchTerms
	 *            a list of match terms which must match in order. These may be
	 *            instances of {@link ETokenType}, {@link ETokenClass}, or sets
	 *            of them, as well as {@link TokenPattern}s.
	 */
	public TokenPattern repeated(Object... matchTerms) {
		final ITokenPattern[] matchers = convertMatchTerms(matchTerms);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int i = 0;
				int lastFullMatch = stream.getPosition();
				TokenPatternMatch subMatch = createDummyMatch();
				while (true) {
					int matcherIndex = i % matchers.length;
					if (i != 0 && matcherIndex == 0) {
						lastFullMatch = stream.getPosition();
						match.mergeFrom(subMatch);
						subMatch = createDummyMatch();
					}

					if (!matchers[matcherIndex].matches(stream, subMatch)) {
						stream.setPosition(lastFullMatch);
						return true;
					}

					i += 1;
				}
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches the given terms exactly once in the
	 * given order.
	 * 
	 * @param matchTerms
	 *            a list of match terms which must match in order. These may be
	 *            instances of {@link ETokenType}, {@link ETokenClass}, or sets
	 *            of them, as well as {@link TokenPattern}s.
	 */
	public TokenPattern sequence(Object... matchTerms) {
		final ITokenPattern[] matchers = convertMatchTerms(matchTerms);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int beforeSequence = stream.getPosition();
				TokenPatternMatch subMatch = createDummyMatch();
				for (int i = 0; i < matchers.length; i++) {
					if (!matchers[i].matches(stream, subMatch)) {
						stream.setPosition(beforeSequence);
						return false;
					}
				}
				match.mergeFrom(subMatch);
				return true;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches the end of the token stream (i.e.
	 * there are no more tokens to match).
	 */
	public TokenPattern endOfStream() {
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				return stream.isExhausted();
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches the beginning of the token stream
	 * (i.e. no tokens have been consumed so far).
	 */
	public TokenPattern beginningOfStream() {
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				return stream.isAtBeginning();
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches anything but the beginning of the
	 * token stream (i.e. at least one token has been consumed so far).
	 */
	public TokenPattern notAtBeginningOfStream() {
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				return !stream.isAtBeginning();
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches if the last matched token is not
	 * followed by a token that matches the given term. This pattern part does
	 * not consume any tokens.
	 * 
	 * @param matchTerm
	 *            May be an instance of {@link ETokenType}, {@link ETokenClass},
	 *            or a set of them, as well as a {@link TokenPattern}.
	 */
	public TokenPattern notFollowedBy(Object matchTerm) {
		final ITokenPattern matcher = convertMatchTerm(matchTerm);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				IToken token = stream.peekCurrent();
				if (token == null) {
					return true;
				}
				TokenPatternMatch subMatch = createDummyMatch();
				int beforeMatch = stream.getPosition();
				boolean success = !matcher.matches(stream, subMatch);
				stream.setPosition(beforeMatch);
				if (success) {
					match.mergeFrom(subMatch);
				}
				stream.setPosition(beforeMatch);
				return success;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches if the last matched token is not
	 * preceded by a token that matches the given term. This pattern part does
	 * not consume any tokens.
	 * 
	 * NOTE: passing a {@link TokenPattern} to this function will not result in
	 * an exception, but it will not yield the intuitive result! The given
	 * pattern will be matched against the token stream, starting with the token
	 * preceding the current token. It is therefore discouraged to pass a
	 * {@link TokenPattern} to this function as it makes understanding the
	 * pattern difficult.
	 * 
	 * @param matchTerm
	 *            May be an instance of {@link ETokenType}, {@link ETokenClass},
	 *            or a set of them, but usually not a {@link TokenPattern}.
	 */
	public TokenPattern notPrecededBy(Object matchTerm) {
		final ITokenPattern matcher = convertMatchTerm(matchTerm);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int beforeMatch = stream.getPosition();
				TokenPatternMatch subMatch = createDummyMatch();
				boolean success;
				if (stream.moveBack() == null) {
					success = true;
				} else {
					success = !matcher.matches(stream, subMatch);
				}
				stream.setPosition(beforeMatch);
				if (success) {
					match.mergeFrom(subMatch);
				}
				return success;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that tries the given match terms one after the
	 * other and stops as soon as one of them matches.
	 * 
	 * @param matchTerms
	 *            a list of match terms which must match in order. These may be
	 *            instances of {@link ETokenType}, {@link ETokenClass}, or sets
	 *            of them, as well as {@link TokenPattern}s.
	 */
	public TokenPattern alternative(Object... matchTerms) {
		final ITokenPattern[] matchers = convertMatchTerms(matchTerms);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int beforeAlternative = stream.getPosition();
				for (int i = 0; i < matchers.length; i++) {
					TokenPatternMatch subMatch = createDummyMatch();
					if (matchers[i].matches(stream, subMatch)) {
						match.mergeFrom(subMatch);
						return true;
					}
					stream.setPosition(beforeAlternative);
				}
				return false;
			}
		}, null);
		return this;
	}

	/**
	 * Adds a new pattern part that matches the given terms zero times or once
	 * in the given order.
	 * 
	 * @param matchTerms
	 *            a list of match terms which must match in order. These may be
	 *            instances of {@link ETokenType}, {@link ETokenClass}, or sets
	 *            of them, as well as {@link TokenPattern}s.
	 */
	public TokenPattern optional(Object... matchTerms) {
		final ITokenPattern[] matchers = convertMatchTerms(matchTerms);
		submatchers.add(new ITokenSubpatternMatcher() {

			@Override
			public boolean match(TokenStream stream, TokenPatternMatch match) {
				int beforeOptional = stream.getPosition();
				TokenPatternMatch subMatch = createDummyMatch();
				for (int i = 0; i < matchers.length; i++) {
					if (!matchers[i].matches(stream, subMatch)) {
						stream.setPosition(beforeOptional);
						return true;
					}
				}
				match.mergeFrom(subMatch);
				return true;
			}
		}, null);
		return this;
	}

	/** Converts the given match terms to {@link ITokenPattern}s. */
	private ITokenPattern[] convertMatchTerms(Object[] matchTerms) {
		ITokenPattern[] matchers = new ITokenPattern[matchTerms.length];
		for (int i = 0; i < matchTerms.length; ++i) {
			matchers[i] = convertMatchTerm(matchTerms[i]);
		}
		return matchers;
	}

	/** Converts a match term to a matcher. */
	private ITokenPattern convertMatchTerm(final Object matchTerm) {

		if (matchTerm instanceof ITokenPattern) {
			return (ITokenPattern) matchTerm;
		}

		if (matchTerm instanceof ETokenType) {
			return new ITokenPattern() {
				@Override
				public boolean matches(TokenStream stream,
						TokenPatternMatch match) {
					IToken token = stream.next();
					if (token == null) {
						return false;
					}
					return token.getType() == (ETokenType) matchTerm;
				}
			};
		}

		if (matchTerm instanceof ETokenClass) {
			return new ITokenPattern() {
				@Override
				public boolean matches(TokenStream stream,
						TokenPatternMatch match) {
					IToken token = stream.next();
					if (token == null) {
						return false;
					}
					return token.getType().getTokenClass() == (ETokenClass) matchTerm;
				}
			};
		}

		if (matchTerm instanceof Set<?>) {
			final Set<?> set = (Set<?>) matchTerm;
			return new ITokenPattern() {
				@Override
				public boolean matches(TokenStream stream,
						TokenPatternMatch match) {
					IToken token = stream.next();
					if (token == null) {
						return false;
					}
					ETokenType type = token.getType();
					return set.contains(type)
							|| set.contains(type.getTokenClass());
				}
			};
		}

		throw new AssertionError("Unsupported match term of type "
				+ matchTerm.getClass());
	}

	/**
	 * Creates a dummy {@link TokenPatternMatch}, which must be merged with an
	 * actual {@link TokenPatternMatch}.
	 */
	private TokenPatternMatch createDummyMatch() {
		return new TokenPatternMatch(null);
	}

	/**
	 * Matches a part of a {@link TokenPattern}.
	 */
	private static interface ITokenSubpatternMatcher {

		/**
		 * Tries to match the subpattern at the given position in the token
		 * stream.
		 * 
		 * A pattern part that does not match must rewind the stream to the
		 * position it was in at the time this method was called.
		 * 
		 * @return true if the pattern part matched or <code>false</code>
		 *         otherwise.
		 */
		public boolean match(TokenStream stream, TokenPatternMatch match);

	}

}
