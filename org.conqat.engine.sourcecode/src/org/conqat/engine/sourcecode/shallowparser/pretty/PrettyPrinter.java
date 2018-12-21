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
package org.conqat.engine.sourcecode.shallowparser.pretty;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.IShallowEntityVisitor;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Class for pretty printing code based on the shallow parser.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49980 $
 * @ConQAT.Rating YELLOW Hash: 6142811C0D773A118D7F45B59851C614
 */
public class PrettyPrinter implements IShallowEntityVisitor {

	/** Indentation depth in spaces. */
	private static final int INDENTATION_DEPTH = 4;

	/** Extra indent for continued long lines. */
	private static final int LONG_LINE_EXTRA_INDENT = 2 * INDENTATION_DEPTH;

	/** Max length of a line. */
	private static final int MAX_LINE_LENGTH = 80;

	/** The language. */
	private final ELanguage language;

	/** The tokens. */
	private final List<IToken> tokens;

	/** Index into {@link #tokens}. */
	private int tokenIndex = 0;

	/** Builder for output code. */
	private final StringBuilder builder = new StringBuilder();

	/** Current indentation level. */
	private int indent = 0;

	/** Current length of a line. */
	private int currentLineLength = 0;

	/** The previously printed token. */
	private IToken previousToken = null;

	/** Intended spacing between statements. */
	private ESpacing statementSpacing = ESpacing.NONE;

	/** Whether we continue a long line. */
	private boolean inLongLineContinuation = false;

	/**
	 * Whether the currently processed token is the first in the sequence of a
	 * statement.
	 */
	private boolean isFirstTokenInSequence = false;

	/**
	 * Indentation depth for ABAP chained statements. If we are not in a chained
	 * statement, this is 0.
	 */
	private int abapChainedStatementDepth = 0;

	/** Constructor. */
	private PrettyPrinter(List<IToken> tokens, ELanguage language) {
		this.language = language;
		this.tokens = tokens;
	}

	/** Formats and returns the code. */
	private String format() throws ConQATException {
		List<ShallowEntity> entities = ShallowParserFactory.createParser(
				language).parseTopLevel(tokens);
		ShallowEntity.traverse(entities, this);
		return builder.toString();
	}

	/** {@inheritDoc} */
	@Override
	public boolean visit(ShallowEntity entity) {
		setStatementSpacing(ESpacing.NEW_LINE);
		if (language == ELanguage.ABAP
				&& SubTypeNames.VISIBILITY.equals(entity.getSubtype())) {
			setStatementSpacing(ESpacing.EMPTY_LINE);
		}

		appendTokens(entity.ownStartTokens());
		indent += 1;
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public void endVisit(ShallowEntity entity) {
		indent -= 1;
		if (!entity.getChildren().isEmpty()) {
			appendTokens(entity.ownEndTokens());
		}

		if (entity.getType() == EShallowEntityType.META
				&& entity.getSubtype().equals("package")) {
			setStatementSpacing(ESpacing.EMPTY_LINE);
		} else if (language == ELanguage.ABAP
				&& SubTypeNames.VISIBILITY.equals(entity.getSubtype())) {
			setStatementSpacing(ESpacing.EMPTY_LINE);
		} else {
			setStatementSpacing(ESpacing.NEW_LINE);
		}
	}

	/** Sets the statement spacing. */
	private void setStatementSpacing(ESpacing spacing) {
		statementSpacing = maxSpacing(statementSpacing, spacing);
	}

	/** Appends the given tokens into a single line. */
	private void appendTokens(UnmodifiableList<IToken> tokens) {
		isFirstTokenInSequence = true;
		for (IToken token : completeTokens(tokens)) {
			appendToken(token);
			if (token.getType().getTokenClass() != ETokenClass.COMMENT) {
				isFirstTokenInSequence = false;
			}
		}
		inLongLineContinuation = false;
		abapChainedStatementDepth = 0;
	}

	/** Appends a single token to the output. */
	private void appendToken(IToken token) {
		ESpacing spacing = statementSpacing;
		statementSpacing = ESpacing.NONE;

		if (previousToken != null) {
			spacing = maxSpacing(spacing, getPostTokenSpacing(previousToken));
			spacing = maxSpacing(spacing,
					getInterTokenSpacing(previousToken, token));
		}
		previousToken = token;

		boolean noIndent = false;
		if (language == ELanguage.ABAP
				&& token.getType() == ETokenType.TRADITIONAL_COMMENT) {
			noIndent = true;
		}

		spacing = maxSpacing(spacing, getPreTokenSpacing(token));
		realizeSpacing(spacing, noIndent);

		// never format literals (e.g. multiline strings from HEREDOC
		List<String> lines = StringUtils.splitLinesAsList(token.getText());
		if (token.getType().getTokenClass() == ETokenClass.LITERAL) {
			builder.append(token.getText());
			if (lines.size() == 1) {
				currentLineLength += lines.get(0).length();
			} else {
				currentLineLength = CollectionUtils.getLast(lines).length();
			}
			return;
		}

		String additionalIndent = StringUtils.EMPTY_STRING;
		if (language == ELanguage.JAVA
				&& token.getType().getTokenClass() == ETokenClass.COMMENT) {
			additionalIndent = StringUtils.SPACE;
		}

		boolean upcase = false;
		boolean lowcase = false;

		if (language == ELanguage.ABAP) {
			if (token.getType().getTokenClass() == ETokenClass.KEYWORD) {
				upcase = true;
			} else if (token.getType().getTokenClass() == ETokenClass.IDENTIFIER) {
				lowcase = true;
			}
		}

		boolean first = true;
		for (String line : lines) {
			if (!first) {
				realizeSpacing(ESpacing.NEW_LINE, noIndent);
				builder.append(additionalIndent);
				currentLineLength += additionalIndent.length();
			}
			first = false;
			String text = line.trim();

			if (upcase) {
				text = text.toUpperCase();
			}
			if (lowcase) {
				text = text.toLowerCase();
			}

			builder.append(text);
			currentLineLength += text.length();
		}
	}

	/** Returns the spacing to be used before the given token. */
	private ESpacing getPreTokenSpacing(IToken token) {
		ETokenType tokenType = token.getType();
		ETokenClass tokenClass = tokenType.getTokenClass();

		if (tokenType == ETokenType.DOCUMENTATION_COMMENT) {
			return ESpacing.EMPTY_LINE;
		}

		if (tokenClass == ETokenClass.COMMENT
				&& (isMultiLine(token) || language == ELanguage.ABAP)) {
			return ESpacing.NEW_LINE;
		}

		if (tokenType == ETokenType.LBRACE) {
			return ESpacing.SPACE;
		}

		// long lines
		if (currentLineLength + token.getText().length() > MAX_LINE_LENGTH
				&& !isFirstTokenInSequence) {
			if (tokenClass != ETokenClass.DELIMITER
					&& tokenClass != ETokenClass.OPERATOR) {
				inLongLineContinuation = true;
				return ESpacing.NEW_LINE;
			}
		}

		return ESpacing.NONE;
	}

	/** Returns the spacing to be used between the given tokens. */
	private ESpacing getInterTokenSpacing(IToken token1, IToken token2) {
		ETokenType tokenType1 = token1.getType();
		ETokenType tokenType2 = token2.getType();
		ETokenClass tokenClass1 = tokenType1.getTokenClass();
		ETokenClass tokenClass2 = tokenType2.getTokenClass();

		// preserve empty lines
		int token1EndLine = token1.getLineNumber()
				+ StringUtils.countLines(token1.getText()) - 1;
		if (token2.getLineNumber() > token1EndLine + 1) {
			return ESpacing.EMPTY_LINE;
		}

		if (language == ELanguage.ABAP && tokenType2 == ETokenType.COLON) {
			abapChainedStatementDepth = currentLineLength + 2;
			return ESpacing.NONE;
		}

		// no space around ABAP method separators
		EnumSet<ETokenType> methodSeparators = EnumSet.of(ETokenType.ARROW,
				ETokenType.EQGT, ETokenType.TILDE);
		if (language == ELanguage.ABAP
				&& (methodSeparators.contains(tokenType1) || methodSeparators
						.contains(tokenType2))) {
			return ESpacing.NONE;
		}

		// No space around generics
		if (tokenType1 == ETokenType.LT || tokenType2 == ETokenType.LT
				|| tokenType2 == ETokenType.GT) {
			return ESpacing.NONE;
		}

		// No space in annotation names
		if (tokenType1 == ETokenType.AT_OPERATOR) {
			return ESpacing.NONE;
		}

		if (tokenClass1 != ETokenClass.DELIMITER
				&& tokenClass2 != ETokenClass.DELIMITER) {
			return ESpacing.SPACE;
		}

		return ESpacing.NONE;
	}

	/** Returns the spacing to be used after the given token. */
	private ESpacing getPostTokenSpacing(IToken token) {
		ETokenType tokenType = token.getType();
		ETokenClass tokenClass = tokenType.getTokenClass();

		if (language == ELanguage.ABAP && tokenType == ETokenType.COMMA
				&& abapChainedStatementDepth > 0) {
			return ESpacing.NEW_LINE;
		}

		if (tokenType == ETokenType.DOCUMENTATION_COMMENT
				|| tokenType == ETokenType.END_OF_LINE_COMMENT) {
			return ESpacing.NEW_LINE;
		}

		if (tokenClass == ETokenClass.COMMENT && isMultiLine(token)) {
			return ESpacing.NEW_LINE;
		}

		if (language == ELanguage.ABAP
				&& tokenType == ETokenType.TRADITIONAL_COMMENT) {
			return ESpacing.NEW_LINE;
		}

		if (tokenType == ETokenType.COMMA || tokenType == ETokenType.SEMICOLON) {
			return ESpacing.SPACE;
		}

		return ESpacing.NONE;
	}

	/** Returns if the token is a multiline token. */
	private boolean isMultiLine(IToken token) {
		return StringUtils.countLines(token.getText()) > 1;
	}

	/** Returns the larger of two spacings. */
	private static ESpacing maxSpacing(ESpacing spacing1, ESpacing spacing2) {
		if (spacing1.ordinal() > spacing2.ordinal()) {
			return spacing1;
		}
		return spacing2;
	}

	/** Realizes the given spacing. */
	private void realizeSpacing(ESpacing spacing, boolean noIndent) {
		// no spacing at start of output
		if (builder.length() == 0) {
			return;
		}

		switch (spacing) {
		case NONE:
			break;
		case SPACE:
			builder.append(StringUtils.SPACE);
			currentLineLength += 1;
			break;
		case EMPTY_LINE:
			builder.append(StringUtils.CR);
			// fallthrough intended
		case NEW_LINE:
			builder.append(StringUtils.CR);
			if (noIndent) {
				currentLineLength = 0;
			} else {
				int spaceCount = indent * INDENTATION_DEPTH;
				if (abapChainedStatementDepth > 0) {
					spaceCount = abapChainedStatementDepth;
				}
				if (inLongLineContinuation) {
					spaceCount += LONG_LINE_EXTRA_INDENT;
				}

				builder.append(StringUtils.fillString(spaceCount,
						StringUtils.SPACE_CHAR));
				currentLineLength = spaceCount;
			}
			break;
		default:
			CCSMAssert.fail("Unknown spacing: " + spacing);
		}
	}

	/** Completes the given tokens with suppressed tokens from {@link #tokens}. */
	private List<IToken> completeTokens(List<IToken> tokens) {
		List<IToken> result = new ArrayList<>();
		for (IToken token : tokens) {
			while (tokenIndex < this.tokens.size() && this.tokens.get(tokenIndex) != token) {
				result.add(this.tokens.get(tokenIndex));
				tokenIndex += 1;
			}
			result.add(token);
			tokenIndex += 1;
		}
		return result;
	}

	/** Formats the code of the given element. */
	public static String format(ITokenElement element) throws ConQATException {
		return format(element.getUnfilteredTextContent(), element.getLanguage());
	}

	/** Formats the given code. */
	public static String format(String code, ELanguage language)
			throws ConQATException {
		return new PrettyPrinter(ScannerUtils.getTokens(code, language),
				language).format();
	}

	/** Enumeration describing possible spacings between tokens. */
	private static enum ESpacing {

		/** No separation. */
		NONE,

		/** Separated by a single space. */
		SPACE,

		/** Separated by a new line. */
		NEW_LINE,

		/** Separated by an empty line. */
		EMPTY_LINE
	}
}
