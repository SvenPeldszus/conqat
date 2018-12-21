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
package org.conqat.engine.sourcecode.shallowparser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Utility methods for {@link IToken} lists.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51544 $
 * @ConQAT.Rating YELLOW Hash: 9DF8B98F9436415D4DB2B16764D03A42
 */
public class TokenStreamUtils {

	/** The value returned if nothing was found in the various find methods. */
	public static final int NOT_FOUND = -1;

	/**
	 * Returns the index of the first token of given token type (or
	 * {@link #NOT_FOUND}).
	 */
	public static int find(List<IToken> tokens, ETokenType... tokenTypes) {
		return find(tokens, 0, tokenTypes);
	}

	/**
	 * Returns the index of the first token of given token type not before the
	 * start index (or {@link #NOT_FOUND}).
	 */
	public static int find(List<IToken> tokens, int startIndex,
			ETokenType... tokenTypes) {
		return find(tokens, startIndex, tokens.size(), tokenTypes);
	}

	/**
	 * Returns the index of the first token of given token type not before the
	 * start index and before the end index (or {@link #NOT_FOUND}).
	 */
	public static int find(List<IToken> tokens, int startIndex, int endIndex,
			ETokenType... tokenTypes) {
		EnumSet<ETokenType> set = toEnumSet(tokenTypes);
		startIndex = Math.max(0, startIndex);
		endIndex = Math.min(endIndex, tokens.size());

		for (int i = startIndex; i < endIndex; ++i) {
			if (set.contains(tokens.get(i).getType())) {
				return i;
			}
		}

		return NOT_FOUND;
	}

	/**
	 * Converts the given token types array into an enum set.
	 */
	private static EnumSet<ETokenType> toEnumSet(ETokenType... tokenTypes) {
		EnumSet<ETokenType> set = EnumSet.noneOf(ETokenType.class);
		set.addAll(Arrays.asList(tokenTypes));
		return set;
	}

	/**
	 * Returns the index of the last token of given token type (or
	 * {@link #NOT_FOUND}).
	 */
	public static int findLast(List<IToken> tokens, ETokenType... tokenTypes) {
		return findLast(tokens, 0, tokenTypes);
	}

	/**
	 * Returns the index of the last token of given token type not before the
	 * start index (or {@link #NOT_FOUND}).
	 */
	public static int findLast(List<IToken> tokens, int startIndex,
			ETokenType... tokenTypes) {
		return findLast(tokens, startIndex, tokens.size(), tokenTypes);
	}

	/**
	 * Returns the index of the last token of given token type not before the
	 * start index and before the end index (or {@link #NOT_FOUND}).
	 */
	public static int findLast(List<IToken> tokens, int startIndex,
			int endIndex, ETokenType... tokenTypes) {
		EnumSet<ETokenType> set = toEnumSet(tokenTypes);
		startIndex = Math.max(0, startIndex);
		endIndex = Math.min(endIndex, tokens.size());

		for (int i = endIndex - 1; i >= startIndex; --i) {
			if (set.contains(tokens.get(i).getType())) {
				return i;
			}
		}

		return NOT_FOUND;
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a token of
	 * <code>tokenType<code>, false otherwise.
	 */
	public static boolean tokenStreamContains(List<IToken> tokens,
			ETokenType tokenType) {
		return tokenStreamContains(tokens, 0, tokens.size(), tokenType);
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a token of
	 * <code>tokenType<code> in the given range, false otherwise.
	 */
	public static boolean tokenStreamContains(List<IToken> tokens,
			int startIndex, int endIndex, ETokenType tokenType) {
		return find(tokens, startIndex, endIndex, tokenType) != NOT_FOUND;
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a any token of
	 * given <code>tokenTypes<code>, false otherwise.
	 */
	public static boolean containsAny(List<IToken> tokens,
			ETokenType... tokenTypes) {
		return containsAny(tokens, 0, tokens.size(), tokenTypes);
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a any token of
	 * given <code>tokenTypes<code> in the given range, false otherwise.
	 */
	public static boolean containsAny(List<IToken> tokens, int startIndex,
			int endIndex, ETokenType... tokenTypes) {
		return containsAny(tokens, startIndex, endIndex, toEnumSet(tokenTypes));
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a any token of
	 * given <code>tokenTypes<code> in the given range, false otherwise.
	 */
	public static boolean containsAny(List<IToken> tokens, int startIndex,
			int endIndex, EnumSet<ETokenType> tokenTypes) {
		if (tokenTypes.isEmpty()) {
			return false;
		}

		for (int i = startIndex; i < endIndex; ++i) {
			if (tokenTypes.contains(tokens.get(i).getType())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns <code>true</code> if the given token stream contains one of the
	 * tokens in the given set.
	 */
	public static boolean containsAny(List<IToken> tokens,
			EnumSet<ETokenType> tokenTypeSet) {
		return containsAny(tokens, 0, tokens.size(), tokenTypeSet);
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains at least one
	 * token of each of the given <code>tokenTypes<code>, false otherwise.
	 */
	public static boolean containsAll(List<IToken> tokens,
			ETokenType... tokenTypes) {
		return containsAll(tokens, 0, tokens.size(), tokenTypes);
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains at least one
	 * token of each of the given
	 * <code>tokenTypes<code> in the given range, false otherwise.
	 */
	public static boolean containsAll(List<IToken> tokens, int startIndex,
			int endIndex, ETokenType... tokenTypes) {
		if (tokenTypes.length == 0) {
			return true;
		}

		EnumSet<ETokenType> types = EnumSet.of(tokenTypes[0], tokenTypes);
		for (int i = startIndex; i < endIndex; ++i) {
			types.remove(tokens.get(i).getType());
			if (types.isEmpty()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a consecutive
	 * subsequence of tokens that match the sequence of token types., false
	 * otherwise.
	 */
	public static boolean containsSequence(List<IToken> tokens, int startIndex,
			int endIndex, ETokenType... tokenTypes) {
		OUTER: for (int i = startIndex; i <= endIndex - tokenTypes.length; ++i) {
			for (int j = 0; j < tokenTypes.length; ++j) {
				if (tokens.get(i + j).getType() != tokenTypes[j]) {
					continue OUTER;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Returns <code>true</code> if the list of tokens contains a token of
	 * <code>tokenClass<code>, false otherwise.
	 */
	public static boolean tokenStreamContains(List<IToken> tokens,
			ETokenClass tokenClass) {
		for (IToken token : tokens) {
			if (token.getType().getTokenClass() == tokenClass) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns the sublist of tokens between the first occurrence of given start
	 * token type and the first occurrence of end token type (after start). If
	 * one of them is not found, an empty list is returned. The tokens for the
	 * start and end are not included in the returned sub list.
	 */
	public static List<IToken> tokensBetween(List<IToken> tokens,
			ETokenType startType, ETokenType endType) {
		int start = TokenStreamUtils.find(tokens, startType);
		if (start == NOT_FOUND) {
			return CollectionUtils.emptyList();
		}
		start += 1;

		int end = TokenStreamUtils.find(tokens, start, endType);
		if (end == NOT_FOUND) {
			return CollectionUtils.emptyList();
		}

		return tokens.subList(start, end);
	}

	/**
	 * Returns the offset of the first token of closingType, thereby counting
	 * nesting occurrences of openingType and closingType. Returns
	 * {@link RecognizerBase#NO_MATCH} if none was found.
	 * 
	 * @param currentOffset
	 *            this is the offset to start the search and must be one token
	 *            <b>after</b> the opening token for which the closing token
	 *            shall be found.
	 */
	public static int findMatchingClosingToken(List<IToken> tokens,
			int currentOffset, ETokenType openingType, ETokenType closingType) {
		int nesting = 1;
		for (; currentOffset < tokens.size(); ++currentOffset) {
			ETokenType tokenType = tokens.get(currentOffset).getType();
			if (tokenType == openingType) {
				nesting += 1;
			} else if (tokenType == closingType) {
				nesting -= 1;
				if (nesting == 0) {
					return currentOffset;
				}
			}
		}
		return NOT_FOUND;
	}

	/**
	 * Returns the offset of the first token of openingType, thereby counting
	 * nesting occurrences of openingType and closingType. Returns
	 * {@link RecognizerBase#NO_MATCH} if none was found.
	 * 
	 * @param currentOffset
	 *            this is the offset to start the search and must be one token
	 *            <b>after</b> the opening token for which the closing token
	 *            shall be found.
	 */
	public static int findMatchingOpeningToken(List<IToken> tokens,
			int currentOffset, ETokenType openingType, ETokenType closingType) {
		int openBraces = 1;
		for (; currentOffset >= 0; currentOffset--) {
			ETokenType currentTokenType = tokens.get(currentOffset).getType();
			if (currentTokenType.equals(openingType)) {
				openBraces--;
			} else if (currentTokenType.equals(closingType)) {
				openBraces++;
			}
			if (openBraces == 0) {
				return currentOffset;
			}
		}
		return NOT_FOUND;
	}

	/**
	 * Returns whether the next tokens starting at the given offset are of given
	 * type.
	 */
	public static boolean tokenTypesAt(List<IToken> tokens, int currentOffset,
			ETokenType... tokenTypes) {
		for (int i = 0; i < tokenTypes.length; ++i) {
			if (currentOffset + i >= tokens.size()
					|| tokens.get(currentOffset + i).getType() != tokenTypes[i]) {
				return false;
			}
		}
		return true;
	}

	/** Returns whether the given token list starts with given token types. */
	public static boolean startsWith(List<IToken> tokens, ETokenType... types) {
		if (tokens.size() < types.length) {
			return false;
		}
		for (int i = 0; i < types.length; ++i) {
			if (tokens.get(i).getType() != types[i]) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Get the type of a variable, designed for JAVA. Mainly this means using
	 * the token prior to the variable as type. Handles arrays "int[]" and
	 * parameterized types "List&lt;Bar&gt;" and returns the braces including
	 * the types in the parameter.
	 * 
	 * @param index
	 *            points to the variable's name
	 */
	public static String getType(List<IToken> tokenList, int index) {
		int startIndex = index - 1;
		while (startIndex >= 2) {
			ETokenType startType = tokenList.get(startIndex).getType();
			if (startType == ETokenType.RBRACK) {
				startIndex -= 2;
			} else if (startType == ETokenType.GT) {
				startIndex = findMatchingOpeningToken(tokenList,
						startIndex - 1, ETokenType.LT, ETokenType.GT) - 1;
			} else if (startType == ETokenType.IDENTIFIER
					&& startIndex > 0
					&& tokenList.get(startIndex - 1).getType() == ETokenType.DOT) {
				startIndex -= 2;
			} else {
				break;
			}
		}

		StringBuilder returnTypeBuilder = new StringBuilder();
		for (int i = startIndex; i < index; i++) {
			returnTypeBuilder.append(tokenList.get(i).getText());
		}
		return returnTypeBuilder.toString();
	}

	/** Counts the number of tokens of a given type in the list of tokens */
	public static int count(List<IToken> tokens, ETokenType tokenType) {
		int counter = 0;
		for (IToken token : tokens) {
			if (token.getType().equals(tokenType)) {
				counter++;
			}
		}
		return counter;
	}

	/**
	 * Turns a token stream into its text string. Tokens are separated by a
	 * single space. Thus, the resulting text is not guaranteed to be the same
	 * as the parsed text from which the tokens resulted.
	 */
	public static String toString(List<IToken> tokens) {
		StringBuilder sb = new StringBuilder();
		for (IToken token : tokens) {
			sb.append(token.getText()).append(StringUtils.SPACE_CHAR);
		}
		return sb.toString();
	}

	/**
	 * Returns the indices into the token list which contain tokens of the given
	 * types.
	 */
	public static List<Integer> findAll(List<IToken> tokens,
			EnumSet<ETokenType> types) {
		List<Integer> indices = new ArrayList<Integer>();
		for (int i = 0; i < tokens.size(); i++) {
			IToken token = tokens.get(i);
			if (types.contains(token.getType())) {
				indices.add(i);
			}
		}
		return indices;
	}

	/** Splits the given token stream at the tokens of the given types. */
	public static List<List<IToken>> split(List<IToken> tokens,
			EnumSet<ETokenType> splitTypes) {
		return split(tokens, splitTypes, Integer.MAX_VALUE);
	}

	/** Splits the given token stream at the tokens of the given types. */
	public static List<List<IToken>> split(List<IToken> tokens,
			ETokenType... splitTypes) {
		EnumSet<ETokenType> set = EnumSet.noneOf(ETokenType.class);
		set.addAll(Arrays.asList(splitTypes));
		return split(tokens, set, Integer.MAX_VALUE);
	}

	/**
	 * Splits the given token stream at the tokens of the given types, but at
	 * most as often as given in the limit parameter. The resulting list will
	 * have at most <code>limit</code> entries). The limit must be bigger than
	 * 0.
	 */
	public static List<List<IToken>> split(List<IToken> tokens,
			EnumSet<ETokenType> splitTypes, int limit) {
		CCSMPre.isTrue(limit > 0, "The limit must be greater than 0");
		List<List<IToken>> parts = new ArrayList<List<IToken>>();
		int start = 0;

		for (int i = 0; i < tokens.size(); i++) {
			if (parts.size() == limit - 1) {
				break;
			}
			if (splitTypes.contains(tokens.get(i).getType())) {
				List<IToken> part = tokens.subList(start, i);
				parts.add(part);
				start = i + 1;
			}
		}

		parts.add(tokens.subList(start, tokens.size()));
		return parts;
	}
}
