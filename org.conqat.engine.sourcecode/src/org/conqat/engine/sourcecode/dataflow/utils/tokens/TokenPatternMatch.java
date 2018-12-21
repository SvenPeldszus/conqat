/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenPatternMatch.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import java.util.ArrayList;
import java.util.List;

import org.conqat.lib.commons.collections.ListMap;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.IToken;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;

/**
 * A single match created by a {@link TokenPattern}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 8BB321B65EC1AB6FC0D26DFEB977AC75
 */
public class TokenPatternMatch {

	/** The token stream against which the matches were constructed. */
	private final List<IToken> tokenStream;

	/** The groups of the match. */
	private final ListMap<Integer, Integer> groups = new ListMap<Integer, Integer>();

	/** Constructor. */
	public TokenPatternMatch(List<IToken> tokenStream) {
		this.tokenStream = tokenStream;
	}

	/**
	 * Appends the given tokens to the group with the given index.
	 */
	public void appendToGroup(Integer groupIndex, int inclusiveStartIndex,
			int exclusiveEndIndex) {
		for (int i = inclusiveStartIndex; i < exclusiveEndIndex; i++) {
			groups.add(groupIndex, i);
		}
	}

	/** Returns the text of the tokens in the given group. */
	public List<String> groupTexts(Integer groupIndex) {
		List<IToken> tokens = groupTokens(groupIndex);
		List<String> texts = new ArrayList<String>();
		for (IToken token : tokens) {
			texts.add(token.getText());
		}
		return texts;
	}

	/**
	 * Returns the indices into the token stream in the given group or an empty
	 * list.
	 */
	public List<Integer> groupIndices(Integer groupIndex) {
		List<Integer> tokens = groups.getCollection(groupIndex);
		if (tokens == null) {
			return new ArrayList<Integer>();
		}
		return tokens;
	}

	/**
	 * Returns the tokens in the given group or an empty list.
	 */
	public List<IToken> groupTokens(Integer groupIndex) {
		List<Integer> indices = groupIndices(groupIndex);
		List<IToken> tokens = new ArrayList<IToken>();
		for (Integer index : indices) {
			tokens.add(tokenStream.get(index));
		}
		return tokens;
	}

	/**
	 * Returns the concatenated text of the tokens in the given group. If the
	 * group was not matched, returns the empty string.
	 */
	public String groupString(Integer groupIndex) {
		return StringUtils.concat(groupTexts(groupIndex),
				StringUtils.EMPTY_STRING);
	}

	/**
	 * Returns <code>true</code> if the match contains tokens in the given
	 * group.
	 */
	public boolean hasGroup(Integer groupIndex) {
		return groups.getCollection(groupIndex) != null;
	}

	/** Merges the given match into this match. */
	public void mergeFrom(TokenPatternMatch other) {
		groups.addAll(other.groups);
	}

	/**
	 * Returns a list of all group strings in all matches of the group with the
	 * given index.
	 */
	public static List<String> getAllStrings(List<TokenPatternMatch> matches,
			int groupIndex) {
		List<String> strings = new ArrayList<String>();
		for (TokenPatternMatch match : matches) {
			strings.add(match.groupString(groupIndex));
		}
		return strings;
	}

	/**
	 * Returns a list of all group tokens in all matches of the group with the
	 * given index.
	 */
	public static List<IToken> getAllTokens(List<TokenPatternMatch> matches,
			int groupIndex) {
		List<IToken> tokens = new ArrayList<IToken>();
		for (TokenPatternMatch match : matches) {
			tokens.addAll(match.groupTokens(groupIndex));
		}
		return tokens;
	}

}
