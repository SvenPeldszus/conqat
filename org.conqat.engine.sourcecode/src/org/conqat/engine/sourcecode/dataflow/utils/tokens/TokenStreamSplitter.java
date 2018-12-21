/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenStreamSplitter.java 51562 2015-01-20 12:48:41Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.Token;

/**
 * Splits a token stream into multiple parts.
 * 
 * TODO (BH): Note to self: needs full review in next round
 * 
 * TODO (FS): Maybe we should be using a shallow parser or something similar
 * instead? Not sure if the shallow parser framework would be appropriate.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51562 $
 * @ConQAT.Rating RED Hash: B2ABC7372FFC320D65F323A80CDFE556
 */
public class TokenStreamSplitter {

	/** The sentinel token to use when splitting nested structures. */
	private static final IToken SENTINEL_TOKEN = new SentinelToken();

	/** The split parts. */
	private List<List<IToken>> tokenStreams = new ArrayList<List<IToken>>();

	/** Constructor. */
	public TokenStreamSplitter(List<IToken> tokens) {
		tokenStreams.add(tokens);
	}

	/** Returns the split streams. */
	public List<List<IToken>> getTokenStreams() {
		return tokenStreams;
	}

	/**
	 * Splits all streams at the given open and close tokens, e.g. parentheses.
	 * The part inside the parentheses will become one new stream and the rest
	 * of the original stream as well. In the outside part, the inside part is
	 * replaced by the {@link #SENTINEL_TOKEN} token.
	 * 
	 * For example: <code>a ( b ) c</code> would be split into
	 * <code>a ( sentinel ) c</code> and <code>b</code>.
	 * 
	 * Outer parts are guaranteed to be stored before inner parts in the
	 * {@link #tokenStreams} list.
	 */
	public void splitNested(ETokenType openToken, ETokenType closeToken) {
		List<List<IToken>> splitStreams = new ArrayList<List<IToken>>();
		for (List<IToken> tokens : tokenStreams) {
			List<NestingInfo> infos = findNestingPairs(tokens, openToken,
					closeToken);
			if (infos == null) {
				// something went wrong, don't split
				return;
			}

			for (NestingInfo info : infos) {
				List<IToken> splitTokens = new ArrayList<IToken>();

				int lastAddedToken = info.getOpenTokenIndex();
				for (int i = 0; i < info.getInnerOpenTokenIndices().size(); i++) {
					if (i >= info.getInnerCloseTokenIndices().size()) {
						// the stream is not properly nested
						break;
					}
					splitTokens.addAll(tokens.subList(lastAddedToken + 1, info
							.getInnerOpenTokenIndices().get(i) + 1));
					splitTokens.add(SENTINEL_TOKEN);
					lastAddedToken = info.getInnerCloseTokenIndices().get(i) - 1;
				}
				splitTokens.addAll(tokens.subList(lastAddedToken + 1,
						info.getCloseTokenIndex()));

				splitStreams.add(splitTokens);
			}
		}
		tokenStreams = splitStreams;
	}

	/**
	 * Returns the {@link NestingInfo}s for all pairs of nested structures, e.g.
	 * parentheses.
	 * 
	 * @return <code>null</code> if something went wrong, e.g. the structures
	 *         were not balanced.
	 */
	private List<NestingInfo> findNestingPairs(List<IToken> tokens,
			ETokenType openType, ETokenType closeType) {
		List<NestingInfo> allInfos = new ArrayList<NestingInfo>();
		List<Integer> openPositions = TokenStreamUtils.findAll(tokens,
				EnumSet.of(openType));
		List<Integer> closePositions = TokenStreamUtils.findAll(tokens,
				EnumSet.of(closeType));
		if (openPositions.size() != closePositions.size()) {
			// something went wrong, e.g. parentheses are not balanced
			return null;
		}

		NestingInfo rootInfo = createNestingInfo(-1, tokens, openType,
				closeType);
		allInfos.add(rootInfo);
		for (Integer openPosition : openPositions) {
			NestingInfo info = createNestingInfo(openPosition, tokens,
					openType, closeType);
			if (info == null) {
				return null;
			}
			allInfos.add(info);
		}

		return allInfos;
	}

	/**
	 * Creates a new nesting info that belongs to the open token at the given
	 * position.
	 */
	private NestingInfo createNestingInfo(Integer openPosition,
			List<IToken> tokens, ETokenType openType, ETokenType closeType) {
		NestingInfo info = new NestingInfo();
		info.setOpenTokenIndex(openPosition);

		int level = 0;
		int holeStart = -1;
		for (int i = openPosition + 1; i < tokens.size(); i++) {
			if (tokens.get(i).getType() == openType) {
				level += 1;
				if (level == 1) {
					holeStart = i;
				}
			} else if (tokens.get(i).getType() == closeType) {
				if (level == 0) {
					info.setCloseTokenIndex(i);
					return info;
				} else if (level == 1) {
					CCSMAssert
							.isTrue(holeStart >= 0,
									"Something went wrong algorithmically. holeStart < 0 but level = 1");
					addHole(info, holeStart, i);
				}
				level -= 1;
			}
		}

		// must have been the root info, i.e. the entire stream
		info.setCloseTokenIndex(tokens.size());
		return info;
	}

	/**
	 * Adds a hole to the given NestingInfo.
	 */
	private void addHole(NestingInfo info, int holeStart, int holeEnd) {
		info.getInnerOpenTokenIndices().add(holeStart);
		info.getInnerCloseTokenIndices().add(holeEnd);
	}

	/** Stores information about nested structures in a token stream. */
	private static class NestingInfo {

		/** The index of the "open" token of this nested structure. */
		private int openTokenIndex = 0;

		/** The index of the "close" token of this nested structure. */
		private int closeTokenIndex = 0;

		/** The indices of the "open" tokens inside this nested structure. */
		private final List<Integer> innerOpenTokenIndices = new ArrayList<Integer>();

		/** The indices of the "close" tokens inside this nested structure. */
		private final List<Integer> innerCloseTokenIndices = new ArrayList<Integer>();

		/** Returns the index of the "open" token of this nested structure. */
		public int getOpenTokenIndex() {
			return openTokenIndex;
		}

		/** Sets the index of the "open" token of this nested structure. */
		public void setOpenTokenIndex(int openToken) {
			this.openTokenIndex = openToken;
		}

		/** Returns the index of the "close" token of this nested structure. */
		public int getCloseTokenIndex() {
			return closeTokenIndex;
		}

		/** Sets the index of the "close" token of this nested structure. */
		public void setCloseTokenIndex(int closeToken) {
			this.closeTokenIndex = closeToken;
		}

		/**
		 * Returns the indices of the "open" tokens inside this nested
		 * structure.
		 */
		public List<Integer> getInnerOpenTokenIndices() {
			return innerOpenTokenIndices;
		}

		/**
		 * Returns the indices of the "close" tokens inside this nested
		 * structure.
		 */
		public List<Integer> getInnerCloseTokenIndices() {
			return innerCloseTokenIndices;
		}

	}

	/** The sentinel token. */
	public static class SentinelToken extends Token {

		/** Constructor. */
		public SentinelToken() {
			super(ETokenType.SENTINEL, 0, 0, "sentinel",
					"fake/sentinel/tokenstreamsplitter");
		}

		/** {@inheritDoc} */
		@Override
		public ELanguage getLanguage() {
			// never used
			return null;
		}

		/** {@inheritDoc} */
		@Override
		public IToken newToken(ETokenType type, int offset, int lineNumber,
				String text, String originId) {
			// never used
			return null;
		}

	}

}
