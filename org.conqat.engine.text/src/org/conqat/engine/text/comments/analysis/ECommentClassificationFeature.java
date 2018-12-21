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
package org.conqat.engine.text.comments.analysis;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.text.comments.Comment;
import org.conqat.engine.text.comments.classification.CodeRecognizer;
import org.conqat.engine.text.comments.classification.CoherenceUtils;
import org.conqat.engine.text.identifier.EStopWords;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Enumeration for the set of features used by the comment classifier.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 50879 $
 * @ConQAT.Rating GREEN Hash: A91D8026493BF09EBA68B58BE25F8ABE
 */
public enum ECommentClassificationFeature {

	// IMPORTANT: Order of literals must be kept as it corresponds to the order
	// of the feature in the feature vector of the trained classifiers. If you
	// change anything here, you have to regenerate the classifiers.

	/** Contains copyright or license keywords? */
	COPYRIGHT(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			return COPYRIGHT_PATTERN.matcher(comment.getText()).find();
		}
	},

	/** Whether this is followed by a type keyword. */
	FOLLOWED_BY_TYPE(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			List<IToken> nextTokens = comment.getNextTokens(
					comment.getTokenIndex(), 10);
			return TokenStreamUtils.containsAny(nextTokens, TYPE_KEYWORD);
		}
	},

	/** Whether this is also marked as a documentation comment in the language. */
	DOC_COMMENT(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			return comment.getToken().getType() == ETokenType.DOCUMENTATION_COMMENT;
		}
	},

	/** Whether this contains documentation tags. */
	CONTAINS_DOC_TAGS(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			return DOC_TAG_PATTERN.matcher(comment.getText()).find();
		}
	},

	/**
	 * Returns the depth in the AST (but reset at type levels). This returns 0
	 * for top-level/next to classes, 1 for within class/next to attribute or
	 * method, 2 otherwise (within method).
	 */
	AST_DEPTH(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			return (double) comment.getAstPosition().ordinal();
		}
	},

	/** Distance to next declaration in number of tokens. */
	DECLARATION_DISTANCE(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			return (double) comment.getMethodFinder()
					.getDistanceToNextDefinition(comment.getTokenIndex());
		}
	},

	/** Returns the relative number of non-empty lines that look like code. */
	COMMENTED_CODE(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			ELanguage language = comment.getLanguage();
			String commentText = comment.getText();
			if (language == ELanguage.JAVA) {
				commentText = commentText.replaceFirst("@see.*",
						StringUtils.EMPTY_STRING);
			}

			List<String> lines = StringUtils.splitLinesAsList(commentText);
			int countCodeLines = 0;
			int overallCount = 0;
			for (String line : lines) {
				if (StringUtils.isEmpty(line)) {
					continue;
				}

				overallCount += 1;
				if (CodeRecognizer.isCodeLine(line, language)) {
					countCodeLines++;
				}
			}

			if (overallCount == 0) {
				return 0.;
			}
			return (double) countCodeLines / ((double) overallCount);
		}
	},

/** Number of occurrences of double operators, such as '<<' or '++'. */
	DOUBLE_OPERATOR_COUNT(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			int count = 0;
			Matcher matcher = DOUBLE_OPERATOR_PATTERN
					.matcher(comment.getText());
			while (matcher.find()) {
				count += 1;
			}
			return (double) count;
		}
	},

	/**
	 * Number of keywords found for the given language. This does not count
	 * keywords that are also english stopwords, such as "for", "while", etc.,
	 * as these are likely to also appear in "normal" text.
	 */
	KEYWORD_COUNT(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			ELanguage language = comment.getToken().getLanguage();
			int count = 0;
			for (IToken token : ScannerUtils.getTokens(comment.getText(),
					language)) {
				if (token.getType().getTokenClass() == ETokenClass.KEYWORD
						&& !EStopWords.ENGLISH.isStopWord(token.getText())) {
					count += 1;
				}
			}
			return (double) count;
		}
	},

	/** Ratio of punctation characters. */
	PUNCTATION_CHARACTERS(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			String commentString = comment.getText().replaceAll("\\s+",
					StringUtils.EMPTY_STRING);
			double allCharacters = commentString.length();
			double specialCharacters = commentString.replaceAll(
					"[^\\p{Punct}]", StringUtils.EMPTY_STRING).length();
			return specialCharacters / allCharacters;
		}
	},

	/** Number of word that have been camel cased. */
	CAMEL_CASED_WORDS(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			int count = 0;
			for (String word : CoherenceUtils.getCommentWords(
					comment.getText(), comment.getLanguage())) {
				if (CAMEL_CASE_PATTERN.matcher(word).find()) {
					count += 1;
				}
			}
			return (double) count;
		}
	},

	/** Returns whether the comment correlates with the next method. */
	CONTEXT_CORRELATION(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			return CoherenceUtils.hasContextCorrelation(comment);
		}
	},

	/** The length of the longest repeated character. */
	CHARACTER_REPETITION(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			char previous = 0;
			int sameCount = 0;
			int maxSameCount = 0;
			for (char c : comment.getText().toCharArray()) {
				if (c == previous) {
					sameCount += 1;
				} else {
					sameCount = 1;
				}
				previous = c;
				maxSameCount = Math.max(sameCount, maxSameCount);
			}
			return (double) maxSameCount;
		}
	},

	/** Length of the comment. */
	LENGTH(false) {
		@Override
		public Object extractFromComment(Comment comment) {
			return (double) StringUtils.countLines(comment.getText());
		}
	},

	/** Followed by another comment */
	FOLLOWED_BY_COMMENT(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			List<IToken> nextTokens = comment.getNextTokens(
					comment.getTokenIndex(), 3);
			for (IToken token : nextTokens) {
				if (token.getType().getTokenClass().equals(ETokenClass.COMMENT)) {
					return true;
				}
			}
			return false;
		}
	},

	/** Is first comment in file. */
	IS_FIRST(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			for (int i = 0; i < comment.getTokenIndex(); i++) {
				if (comment.getTokens().get(i).getType().getTokenClass()
						.equals(ETokenClass.COMMENT)) {
					return false;
				}
			}
			return true;
		}
	},

	/**
	 * Returns whether this looks like a section, i.e. there are at most two
	 * words and the last word references program elements, or the entire text
	 * stars with "start/begin/end of".
	 */
	SECTION_LIKE(true) {
		@Override
		public Object extractFromComment(Comment comment) {
			List<String> words = CoherenceUtils.getCommentWords(
					comment.getText(), comment.getLanguage());
			return words.isEmpty()
					|| (words.size() <= 3 && SECTION_END_WORDS
							.contains(CollectionUtils.getLast(words)
									.toLowerCase()))
					|| (words.size() > 2
							&& SECTION_START_WORDS.contains(words.get(0)
									.toLowerCase()) && words.get(1)
							.equalsIgnoreCase("of"));
		}
	};

	/** Pattern for the {@link #COPYRIGHT} feature. */
	private static final Pattern COPYRIGHT_PATTERN = Pattern.compile(
			"copyright|license", Pattern.CASE_INSENSITIVE);

	/** Pattern for the {@link #CONTAINS_DOC_TAGS} feature. */
	private static final Pattern DOC_TAG_PATTERN = Pattern
			.compile("[@\\<](param|return|link|throw|inherit)");

	/** Set of token types representing a class, interface, enum, or struct. */
	private static final ETokenType[] TYPE_KEYWORD = { ETokenType.CLASS,
			ETokenType.INTERFACE, ETokenType.ENUM, ETokenType.STRUCT };

	/** Matches double operator. */
	private static final Pattern DOUBLE_OPERATOR_PATTERN = Pattern
			.compile("(^|[^\\p{Punct}])[\\p{Punct}&&[^.]]{2}($|[^\\p{Punct}])");

	/** Pattern used for recognizing camel-cased words. */
	private static final Pattern CAMEL_CASE_PATTERN = Pattern
			.compile("[a-z][A-Z]");

	/**
	 * Words that hint strongly at a section context when found at the start
	 * (all lowercase).
	 */
	private static final Set<String> SECTION_START_WORDS = new HashSet<>(
			Arrays.asList("begin", "start", "end"));

	/**
	 * Words that hint strongly at a section context when found at the end (all
	 * lowercase).
	 */
	private static final Set<String> SECTION_END_WORDS = new HashSet<>(
			Arrays.asList("classes", "variables", "attributes", "constructors",
					"methods", "events", "listeners", "handlers", "ctors",
					"fields", "properties", "implementation"));

	/** Whether this is a boolean feature (or else a double feature) */
	private final boolean isBooleanFeature;

	/** Constructor */
	private ECommentClassificationFeature(boolean isBooleanFeature) {
		this.isBooleanFeature = isBooleanFeature;
	}

	/** Returns isBooleanFeature. */
	public boolean isBooleanFeature() {
		return isBooleanFeature;
	}

	/**
	 * Extracts a value of this feature from the given comment. Results are
	 * either of type boolean or double depending on the feature. Use
	 * {@link #isBooleanFeature()} to determine this.
	 */
	public abstract Object extractFromComment(Comment comment);

}
