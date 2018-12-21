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
package org.conqat.engine.sourcecode.shallowparser.languages.cpp;

import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_TYPE;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.TOP_LEVEL;
import static org.conqat.lib.scanner.ETokenType.AND;
import static org.conqat.lib.scanner.ETokenType.ASSERT;
import static org.conqat.lib.scanner.ETokenType.BOOL;
import static org.conqat.lib.scanner.ETokenType.BREAK;
import static org.conqat.lib.scanner.ETokenType.BYTE;
import static org.conqat.lib.scanner.ETokenType.CASE;
import static org.conqat.lib.scanner.ETokenType.CHAR;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.CONST;
import static org.conqat.lib.scanner.ETokenType.CONTINUE;
import static org.conqat.lib.scanner.ETokenType.DELETE;
import static org.conqat.lib.scanner.ETokenType.DOUBLE;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ENUM;
import static org.conqat.lib.scanner.ETokenType.EQ;
import static org.conqat.lib.scanner.ETokenType.EXTERN;
import static org.conqat.lib.scanner.ETokenType.FAR;
import static org.conqat.lib.scanner.ETokenType.FINAL;
import static org.conqat.lib.scanner.ETokenType.FLOAT;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.GOTO;
import static org.conqat.lib.scanner.ETokenType.GT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.INT;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LONG;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.MINUSMINUS;
import static org.conqat.lib.scanner.ETokenType.MULT;
import static org.conqat.lib.scanner.ETokenType.NAMESPACE;
import static org.conqat.lib.scanner.ETokenType.NEAR;
import static org.conqat.lib.scanner.ETokenType.NEW;
import static org.conqat.lib.scanner.ETokenType.PLUSPLUS;
import static org.conqat.lib.scanner.ETokenType.PREPROCESSOR_DIRECTIVE;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RETURN;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SCOPE;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.SHORT;
import static org.conqat.lib.scanner.ETokenType.SIGNED;
import static org.conqat.lib.scanner.ETokenType.STATIC;
import static org.conqat.lib.scanner.ETokenType.STRUCT;
import static org.conqat.lib.scanner.ETokenType.SUPER;
import static org.conqat.lib.scanner.ETokenType.SWITCH;
import static org.conqat.lib.scanner.ETokenType.THIS;
import static org.conqat.lib.scanner.ETokenType.THROW;
import static org.conqat.lib.scanner.ETokenType.TYPEDEF;
import static org.conqat.lib.scanner.ETokenType.TYPENAME;
import static org.conqat.lib.scanner.ETokenType.UNION;
import static org.conqat.lib.scanner.ETokenType.UNSIGNED;
import static org.conqat.lib.scanner.ETokenType.USING;
import static org.conqat.lib.scanner.ETokenType.VIRTUAL;
import static org.conqat.lib.scanner.ETokenType.VOID;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.CStyleShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Shallow parser for C/C++.
 * <p>
 * What this parser does and does not:
 * <ul>
 * <li>The parser recognizes types (classes, enums, interfaces), methods and
 * attributes, and individual statements.</li>
 * <li>It recognizes the nesting of statements (e.g. in loops), but does not
 * parse into the statements. For example, it recognizes an if-statement and
 * provides the list of sub-statements, but does not provide direct access to
 * the if-condition.</li>
 * <li>All preprocessor statements are parsed as meta information.</li>
 * <li>Template declarations are parsed as preceding meta information.</li>
 * <li>Forward declarations are handled as meta information.</li>
 * <li>We heuristically filter code generating macros, such as
 * "CREATE_STUFF(MyClass)".</li>
 * </ul>
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49553 $
 * @ConQAT.Rating GREEN Hash: 2A59E2B50B74B49E1455F07FD5C0465C
 */
public class CppShallowParser extends CStyleShallowParserBase {

	/** Keywords used for primitive types. */
	/* package */static final EnumSet<ETokenType> PRIMITIVE_TYPES = EnumSet.of(
			VOID, BYTE, SHORT, INT, LONG, FLOAT, DOUBLE, SIGNED, UNSIGNED,
			CHAR, BOOL);

	/**
	 * Set of common "keywords" that are not actually part of the language but
	 * rather used by certain compilers and implicitly defined using macros. The
	 * solution used here is to filter them out.
	 */
	private static final Set<String> PSEUDO_KEYWORDS = new HashSet<String>(
			Arrays.asList(
					// typically found in Windows compilers
					"__fastcall", "__export", "__forceinline", "_cdecl",
					"_stdcall", "__stdcall", "WINAPI", "APIENTRY", "CALLBACK",
					// used by the common Qt library
					"Q_EXPORT",
					// keywords found in ISA dialog manager
					"DML_c", "DM_CALLBACK", "__1", "__2", "__3", "__4", "__5",
					"__6", "__7", "DM_ENTRY", "DML_pascal", "DML_default",
					// project specific keywords
					"IGVPWORD_API"));

	/** Constructor. */
	public CppShallowParser() {
		createNamespaceRules();
	}

	/** {@inheritDoc} */
	@Override
	protected void createMetaRules() {
		new CppShallowParserMetaRules(this).contributeRules();
		super.createMetaRules();
	}

	/** Creates namespace specific rules. */
	private void createNamespaceRules() {
		// using
		inAnyState().sequence(USING).createNode(EShallowEntityType.META, 0)
				.skipTo(SEMICOLON).endNode();

		// namespace
		RecognizerBase<EGenericParserStates> namespaceAlternative = inAnyState()
				.sequence(NAMESPACE).skipBefore(EnumSet.of(SEMICOLON, LBRACE));
		namespaceAlternative.sequence(LBRACE)
				.createNode(EShallowEntityType.MODULE, 0, new Region(1, -2))
				.parseUntil(TOP_LEVEL).sequence(RBRACE).endNode();
		namespaceAlternative.sequence(SEMICOLON)
				.createNode(EShallowEntityType.META, 0, new Region(1, -2))
				.endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected void createTypeRules() {
		// typedef
		RecognizerBase<EGenericParserStates> typeInTypedefAlternative = inAnyState()
				.sequence(TYPEDEF).optional(CONST);
		typeInTypedefAlternative
				.sequenceBefore(getTypeKeywords(), IDENTIFIER, LBRACE)
				.createNode(EShallowEntityType.TYPE, 0).parseOnce(TOP_LEVEL)
				.skipTo(IDENTIFIER, SEMICOLON).endNodeWithName(-2);
		typeInTypedefAlternative.sequenceBefore(getTypeKeywords(), LBRACE)
				.createNode(EShallowEntityType.TYPE, 0).parseOnce(TOP_LEVEL)
				.skipTo(IDENTIFIER, SEMICOLON).endNodeWithName(-2);

		RecognizerBase<EGenericParserStates> simpleTypedefAlternative = inAnyState()
				.sequence(TYPEDEF).createNode(EShallowEntityType.TYPE, 0)
				.skipBefore(IDENTIFIER, EnumSet.of(SEMICOLON, RPAREN));
		simpleTypedefAlternative.sequence(IDENTIFIER, SEMICOLON)
				.endNodeWithName(-2);
		simpleTypedefAlternative.markStart().sequence(IDENTIFIER, RPAREN)
				.skipTo(SEMICOLON).endNodeWithName(0);

		// enum (both anonymous and named)
		RecognizerBase<EGenericParserStates> enumAlternative = inAnyState()
				.sequence(ENUM);
		enumAlternative.sequence(LBRACE).createNode(EShallowEntityType.TYPE, 0)
				.skipTo(RBRACE).optional(SEMICOLON).endNode();
		enumAlternative.sequence(IDENTIFIER, EnumSet.of(LBRACE, COLON))
				.createNode(EShallowEntityType.TYPE, 0, 1).skipTo(RBRACE)
				.optional(SEMICOLON).endNode();

		// types; we have to ensure when skipping to the LBRACE, that there is
		// no earlier SEMICOLON or EQ, as in these cases it is a forward
		// declaration or a variable.
		inAnyState().sequence(getTypeKeywords(), getValidIdentifiers())
				.skipBefore(EnumSet.of(SEMICOLON, LBRACE, EQ)).sequence(LBRACE)
				.createNode(EShallowEntityType.TYPE, 0, 1).parseUntil(IN_TYPE)
				.sequence(RBRACE).optional(SEMICOLON).endNode();

		// anonymous types
		inAnyState().sequence(getTypeKeywords(), LBRACE)
				.createNode(EShallowEntityType.TYPE, 0, "<anonymous>")
				.parseUntil(IN_TYPE).sequence(RBRACE).optional(SEMICOLON)
				.endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getTypeKeywords() {
		return EnumSet.of(CLASS, STRUCT, UNION, ENUM);
	}

	/** {@inheritDoc} */
	@Override
	protected void createClassElementsRules() {
		new CppShallowParserClassElementRules(this).contributeRules();
	}

	/**
	 * Creates a new recognizer that can match a scope prefix for a method-like
	 * construct. This includes sequences of identifiers with double colon,
	 * possibly intermixed with template arguments.
	 */
	/* package */RecognizerBase<EGenericParserStates> createScopeRecognizer() {
		// remember the start of the recognizer chain (we can not used the
		// result of the method chain, as this would be the last recognizer!)
		RecognizerBase<EGenericParserStates> result = emptyRecognizer();
		result.sequence(IDENTIFIER).skipNested(LT, GT).sequence(SCOPE);
		return result;
	}

	/** {@inheritDoc} */
	@Override
	protected void createCaseRule() {
		super.createCaseRule();

		// C/C++ also allows parentheses here and type casts (hence two sets of
		// nested parentheses).
		inState(IN_METHOD).markStart().sequence(CASE).skipTo(COLON)
				.createNode(EShallowEntityType.META, 0).endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected void createSimpleStatementRule() {

		EnumSet<ETokenType> separators = EnumSet.of(LBRACE, RBRACE);
		separators.addAll(ETokenType.KEYWORDS);

		inState(IN_METHOD).sequence(new UppercaseIdentifierMatcher())
				.skipNested(LPAREN, RPAREN).optional(PREPROCESSOR_DIRECTIVE)
				.sequenceBefore(separators)
				.createNode(EShallowEntityType.STATEMENT, "Expanded macro", 0)
				.endNode();

		super.createSimpleStatementRule();
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getSimpleBlockKeywordsWithParentheses() {
		return EnumSet.of(WHILE, FOR, SWITCH);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getSimpleBlockKeywordsWithoutParentheses() {
		return EnumSet.of(ELSE);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getStatementStartTokens() {
		return EnumSet.of(NEW, DELETE, BREAK, CONTINUE, RETURN, ASSERT, FINAL,
				GOTO, SUPER, THIS, THROW, MULT, LPAREN, PLUSPLUS, MINUSMINUS,
				SCOPE);
	}

	/** {@inheritDoc} */
	@Override
	protected RecognizerBase<EGenericParserStates> typePattern(
			RecognizerBase<EGenericParserStates> currentState) {

		EnumSet<ETokenType> extendedTypeKeywords = EnumSet
				.copyOf(getTypeKeywords());
		extendedTypeKeywords.add(TYPENAME);

		EnumSet<ETokenType> typeOrIdentifier = EnumSet.of(IDENTIFIER);
		typeOrIdentifier.addAll(PRIMITIVE_TYPES);

		return currentState
				.repeated(EnumSet.of(CONST, STATIC, VIRTUAL, EXTERN, NEAR, FAR))
				.optional(extendedTypeKeywords)
				.subRecognizer(createScopeRecognizer(), 0, Integer.MAX_VALUE)
				.sequence(typeOrIdentifier)
				.skipNested(LT, GT)
				.skipAny(EnumSet.of(MULT, AND, CONST))
				.skipNested(LBRACK, RBRACK)
				.skipAny(
						EnumSet.of(MULT, AND, CONST, LBRACK, RBRACK, NEAR, FAR));
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFilteredToken(IToken token, IToken previousToken) {
		if (token.getType() == IDENTIFIER
				&& PSEUDO_KEYWORDS.contains(token.getText())) {
			return true;
		}

		return super.isFilteredToken(token, previousToken);
	}

	/** {@inheritDoc} */
	@Override
	protected List<IToken> filterTokens(List<IToken> tokens) {
		return filterGCCAttributes(super.filterTokens(tokens));
	}

	/**
	 * Filters GCC attributes. See e.g.
	 * http://gcc.gnu.org/onlinedocs/gcc/Type-Attributes.html
	 */
	private List<IToken> filterGCCAttributes(List<IToken> tokens) {
		List<IToken> result = new ArrayList<>();
		boolean inAttribute = false;
		int openBraces = 0;
		for (int i = 0; i < tokens.size(); i++) {
			IToken token = tokens.get(i);
			if (token.getText().equals("__attribute__")) {
				inAttribute = true;
			} else if (inAttribute && token.getType() == ETokenType.LPAREN) {
				openBraces++;
			} else if (inAttribute && token.getType() == ETokenType.RPAREN) {
				openBraces--;
				if (openBraces == 0) {
					inAttribute = false;
				}
			} else if (!inAttribute) {
				result.add(token);
			}
		}
		return result;
	}
}
