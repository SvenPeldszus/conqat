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
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.COMP;
import static org.conqat.lib.scanner.ETokenType.GT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.OPERATOR;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.VIRTUAL;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.CStyleShallowParserRuleProviderBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.ETokenType;

/**
 * Provides the class element rules for the {@link CppShallowParser}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: 6129B0C9231F1601AFA65D332F2C6330
 */
/* package */class CppShallowParserClassElementRules extends
		CStyleShallowParserRuleProviderBase<CppShallowParser> {

	/** Constructor. */
	public CppShallowParserClassElementRules(CppShallowParser delegateParser) {
		super(delegateParser);
	}

	/** {@inheritDoc} */
	@Override
	public void contributeRules() {
		createOperatorRules();
		createMethodRules();

		// heuristic for preprocessor additions; we specify it here although we
		// parse it as META, as we want it to match between the constructor and
		// the attribute
		inState(TOP_LEVEL, IN_TYPE)
				.sequence(new UppercaseIdentifierMatcher(), LPAREN)
				.skipToWithNesting(RPAREN, LPAREN, RPAREN)
				.createNode(EShallowEntityType.META,
						"preprocessor generated code", 0).endNode();

		// attributes and global variables
		typePatternInState(IN_TYPE, TOP_LEVEL)
				.subRecognizer(createScopeRecognizer(), 0, Integer.MAX_VALUE)
				.markStart().sequence(IDENTIFIER)
				.createNode(EShallowEntityType.ATTRIBUTE, "attribute", 0)
				.skipToWithNesting(SEMICOLON, LBRACE, RBRACE).endNode();

		// global variables that directly follow a struct/class/enum.
		// Example: struct { int x, y; } myPair = { 16, 17};
		// In this case we want to parse the struct as type and the part from
		// myPair as variable. As these are only common in plain C code, we only
		// use top-level and no namespaces and generics. Heuristic is to look
		// for = or [].
		inState(TOP_LEVEL)
				.sequence(IDENTIFIER,
						EnumSet.of(ETokenType.EQ, ETokenType.LBRACK))
				.createNode(EShallowEntityType.ATTRIBUTE, "global variable", 0)
				.skipToWithNesting(SEMICOLON, LBRACE, RBRACE).endNode();
	}

	/** Creates parsing rules for operator overloading. */
	private void createOperatorRules() {
		// operator overloading
		completeMethod("operator", typePatternInState(IN_TYPE, TOP_LEVEL)
				.subRecognizer(createScopeRecognizer(), 0, Integer.MAX_VALUE)
				.markStart().sequence(OPERATOR).skipTo(LPAREN),
				new Region(0, 1));

		// operator overloading without type (conversion operators)
		completeMethod("cast operator", inState(IN_TYPE, TOP_LEVEL)
				.subRecognizer(createScopeRecognizer(), 0, Integer.MAX_VALUE)
				.markStart().sequence(OPERATOR).skipTo(LPAREN), new int[] { 0,
				1 });
	}

	/**
	 * Creates parsing rules for methods (including constructors and
	 * destructors).
	 */
	private void createMethodRules() {
		// functions, procedures, methods
		completeMethod("function", typePatternInState(IN_TYPE, TOP_LEVEL)
				.subRecognizer(createScopeRecognizer(), 0, Integer.MAX_VALUE)
				.markStart().sequence(IDENTIFIER).sequence(LPAREN));

		// destructor
		completeMethod(
				"destructor",
				inState(IN_TYPE, TOP_LEVEL)
						.optional(VIRTUAL)
						.subRecognizer(createScopeRecognizer(), 0,
								Integer.MAX_VALUE).sequence(COMP).markStart()
						.sequence(IDENTIFIER).skipNested(LT, GT)
						.sequence(LPAREN));

		// constructor
		completeMethod(
				"constructor",
				inState(IN_TYPE, TOP_LEVEL)
						.subRecognizer(createScopeRecognizer(), 0,
								Integer.MAX_VALUE).markStart()
						.sequence(IDENTIFIER).skipNested(LT, GT)
						.sequence(LPAREN));
	}

	/**
	 * Completes a method-like construct. This begins with skipping the
	 * parameter list (i.e. the construct has to already match the left
	 * parenthesis). This end either in a complete method with a body, or with a
	 * semicolon and thus is just a declaration.
	 */
	private void completeMethod(String name,
			RecognizerBase<EGenericParserStates> start) {
		completeMethod(name, start, 0);
	}

	/**
	 * Completes a method-like construct. This begins with skipping the
	 * parameter list (i.e. the construct has to already match the left
	 * parenthesis). This end either in a complete method with a body, or with a
	 * semicolon and thus is just a declaration.
	 * 
	 * @param methodName
	 *            the name of the method. This object accepts the same types as
	 *            the name object in createNode.
	 */
	private void completeMethod(String name,
			RecognizerBase<EGenericParserStates> start, Object methodName) {

		// the keywords we break on to check for K&R style
		EnumSet<ETokenType> krCheck = EnumSet.of(LBRACE, SEMICOLON, IDENTIFIER,
				COLON);
		krCheck.addAll(CppShallowParser.PRIMITIVE_TYPES);

		// the next check is a bit more involved, as it is part of our heuristic
		// to recognize code generating preprocessor statements and also should
		// support K&R style functions
		RecognizerBase<EGenericParserStates> krStyleAlternative = start
				.skipToWithNesting(RPAREN, LPAREN, RPAREN)
				// 1.) go to first LBRACE, SEMICOLON, IDENTIFIER,
				// COLON, or primitive type keyword
				.skipBeforeWithNesting(krCheck, LPAREN, RPAREN);

		// 2.) If we find a type keyword first, this must be K&R style and we
		// continue at the brace
		krStyleAlternative.sequence(CppShallowParser.PRIMITIVE_TYPES)
				.skipTo(LBRACE)
				.createNode(EShallowEntityType.METHOD, name, methodName)
				.parseUntil(IN_METHOD).sequence(RBRACE).endNode();

		RecognizerBase<EGenericParserStates> declarationAlternative = krStyleAlternative
		// 3.) break if it is an IDENTIFIER, as these are only expected after a
		// colon (constructor) or in parentheses (throw decl)
				.sequenceBefore(EnumSet.of(LBRACE, SEMICOLON, COLON))
				// 4.) skip again (in case we stopped at the COLON)
				.skipBefore(EnumSet.of(LBRACE, SEMICOLON));

		// 4.) LBRACE means that this is a definition
		declarationAlternative.sequence(LBRACE)
				.createNode(EShallowEntityType.METHOD, name, methodName)
				.parseUntil(IN_METHOD).sequence(RBRACE).endNode();

		// 5.) SEMICOLON means that this is a declaration
		declarationAlternative
				.sequence(SEMICOLON)
				.createNode(EShallowEntityType.METHOD, name + " declaration",
						methodName).endNode();
	}

	/**
	 * Creates a new recognizer that can match a scope prefix for a method-like
	 * construct. This includes sequences of identifiers with double colon,
	 * possibly intermixed with template arguments.
	 */
	private RecognizerBase<EGenericParserStates> createScopeRecognizer() {
		return delegateParser.createScopeRecognizer();
	}
}
