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
package org.conqat.engine.text.comments.classification;

import java.util.EnumSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Class to detect code snippets in comments
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49705 $
 * @ConQAT.Rating GREEN Hash: AAEF01EC25FDE85EEB0CD18036CF0F5F
 */
public class CodeRecognizer {

	/** Set of languages with C-like syntax. */
	private static final Set<ELanguage> CLIKE_LANGUAGES = EnumSet.of(
			ELanguage.JAVA, ELanguage.CS, ELanguage.CPP, ELanguage.JAVASCRIPT);

	/**
	 * Pattern that is looking for markup, which is defined by special
	 * characters followed by an opening brace.
	 */
	private static final Pattern MARKUP_PATTERN = Pattern
			.compile("[@%][(\\[{]|\\{@");

	/** Pattern that is looking for URLs. */
	private static final Pattern URL_PATTERN = Pattern.compile("\\w+://.*/");

	/** Pattern that is looking for HMTL-like tags. */
	private static final Pattern HTML_TAG_PATTERN = Pattern.compile("</?\\w+>");

	/** Pattern for Java if, for, and while statements */
	private static final Pattern JAVA_CONTROL_LOOP_PATTERN = Pattern
			.compile("(if|while|for)\\s*\\(.*");

	/** Pattern for method/constructor calls. */
	private static final Pattern METHOD_CONSTRUCTOR_CALL_PATTERN = Pattern
			.compile("([_a-zA-Z][_a-zA-Z0-9]*\\.|new\\s+)[_a-zA-Z][_a-zA-Z0-9]*\\s*\\(.*?\\)");

	/** Pattern for recognizing assignments. */
	private static final Pattern ASSIGNMENT_PATTERN = Pattern
			.compile("\\s*([_a-zA-Z0-9]+\\.)*[_a-zA-Z0-9]+\\s*.?=.*;\\s*");

	/** Pattern for recognizing '()' or '[]'. */
	private static final Pattern EMPTY_PARENTHESES_PATTERN = Pattern
			.compile("\\(\\)|\\[\\]");

	/** Pattern for recognizing a line containins only a single brace. */
	private static final Pattern ONLY_BRACE_PATTERN = Pattern
			.compile("^\\s*[{}]\\s*$");

	/** Returns true if the given string contains code snippets. */
	public static boolean isCodeLine(String commentLine, ELanguage language) {
		if (containsMarkup(commentLine) || containsUrl(commentLine)
				|| containsTags(commentLine)) {
			return false;
		}

		if (CLIKE_LANGUAGES.contains(language)
				&& (matchMethodCallPattern(commentLine)
						|| containsJavaControlLoopPattern(commentLine)
						|| isAssignment(commentLine) || isOnlyBrace(commentLine))) {
			return true;
		}

		return hasEmptyParentheses(commentLine)
				|| containsCodeCharacteristic(commentLine, language);
	}

	/** Returns whether the line consists of only a curly brace. */
	private static boolean isOnlyBrace(String commentLine) {
		return ONLY_BRACE_PATTERN.matcher(commentLine).matches();
	}

	/** Returns if the line contains some kind of markup. */
	private static boolean containsMarkup(String commentLine) {
		return MARKUP_PATTERN.matcher(commentLine).find();
	}

	/** Returns if the line contains a URL. */
	private static boolean containsUrl(String commentLine) {
		return URL_PATTERN.matcher(commentLine).find();
	}

	/** Returns if the line contains something looking like a HTML tag. */
	private static boolean containsTags(String commentLine) {
		return HTML_TAG_PATTERN.matcher(commentLine).find();
	}

	/**
	 * Returns true if the given line of comment has a method call pattern.
	 */
	private static boolean matchMethodCallPattern(String commentLine) {
		return METHOD_CONSTRUCTOR_CALL_PATTERN.matcher(commentLine).find();
	}

	/**
	 * Returns true if the given comment line contains java syntax such as an
	 * "if" or a "while" statement.
	 */
	private static boolean containsJavaControlLoopPattern(String commentline) {
		return JAVA_CONTROL_LOOP_PATTERN.matcher(commentline).find();
	}

	/** Returns true if the given comment line looks like an assignment. */
	private static boolean isAssignment(String commentline) {
		return ASSIGNMENT_PATTERN.matcher(commentline).matches();
	}

	/**
	 * Returns true if the given comment line contains empty parentheses or
	 * brackets.
	 */
	private static boolean hasEmptyParentheses(String commentline) {
		return EMPTY_PARENTHESES_PATTERN.matcher(commentline).find();
	}

	/** Returns true is the line consists of a significant number of code parts. */
	private static boolean containsCodeCharacteristic(String commentline,
			ELanguage language) {
		int tokenCount = 0;
		int codeTokenCount = 0;
		for (IToken token : ScannerUtils.getTokens(commentline, language)) {
			if (token.getType() == ETokenType.DOT
					|| token.getType() == ETokenType.COMMA) {
				// ignore the dot as it is too common
				continue;
			}

			switch (token.getType().getTokenClass()) {
			case ERROR:
				// error token indicates non-code
				return false;

			case SPECIAL:
			case WHITESPACE:
			case SYNTHETIC:
				// skip these tokens
				break;
			case DELIMITER:
			case OPERATOR:
			case KEYWORD:
				codeTokenCount += 1;
				// fall-through intended
			default:
				tokenCount += 1;
			}
		}

		return tokenCount > 0 && (codeTokenCount / (double) tokenCount) > .34;
	}
}
