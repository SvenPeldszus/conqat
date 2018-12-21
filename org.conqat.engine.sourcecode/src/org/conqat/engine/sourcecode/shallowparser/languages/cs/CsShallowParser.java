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
package org.conqat.engine.sourcecode.shallowparser.languages.cs;

import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_EXPRESSION;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_MODULE;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_TYPE;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.TOP_LEVEL;
import static org.conqat.lib.scanner.ETokenType.*;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.CStyleShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;

/**
 * Shallow parser for C#
 * <p>
 * What this parser does and does not:
 * <ul>
 * <li>The parser recognizes types (classes, enums, interfaces), methods and
 * attributes, and individual statements.</li>
 * <li>It recognizes the nesting of statements (e.g. in loops), but does not
 * parse into the statements. For example, it recognizes an if-statement and
 * provides the list of sub-statements, but does not provide direct access to
 * the if-condition.</li>
 * <li>Using statements and annotations are parsed as meta information.</li>
 * </ul>
 *
 * @author $Author: hummelb $
 * @version $Rev: 51186 $
 * @ConQAT.Rating GREEN Hash: 82345E489962A5048788E267BEAA5838
 */
public class CsShallowParser extends CStyleShallowParserBase {

	/** {@inheritDoc} */
	@Override
	protected void createMetaRules() {
		// using
		inState(TOP_LEVEL, IN_MODULE).markStart().sequence(EnumSet.of(USING))
				.skipTo(SEMICOLON)
				.createNode(EShallowEntityType.META, 0, new Region(1, -2))
				.endNode();

		// annotations
		inState(IN_TYPE, IN_MODULE, TOP_LEVEL).sequence(LBRACK)
				.createNode(EShallowEntityType.META, "annotation")
				.skipToWithNesting(RBRACK, LBRACK, RBRACK).endNode();

		super.createMetaRules();
	}

	/** {@inheritDoc} */
	@Override
	protected void createTypeRules() {
		// namespace
		inState(TOP_LEVEL, IN_MODULE)
				.sequence(NAMESPACE, getValidIdentifiers()).skipTo(LBRACE)
				.createNode(EShallowEntityType.MODULE, 0, new Region(1, -2))
				.parseUntil(IN_MODULE).sequence(RBRACE).endNode();

		super.createTypeRules();
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getTypeKeywords() {
		return EnumSet.of(CLASS, INTERFACE, ENUM, STRUCT);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getTypeModifier() {
		return EnumSet.of(PUBLIC, PRIVATE, ABSTRACT, SEALED, INTERNAL, PARTIAL);
	}

	/** {@inheritDoc} */
	@Override
	protected void createClassElementsRules() {
		// simple enum literals
		inState(IN_TYPE).sequence(IDENTIFIER)
				.sequenceBefore(EnumSet.of(COMMA, EQ, RBRACE))
				.createNode(EShallowEntityType.ATTRIBUTE, "enum literal", 0)
				.skipBefore(EnumSet.of(COMMA, RBRACE)).optional(COMMA)
				.endNode();

		// delegates
		typePattern(inState(TOP_LEVEL, IN_MODULE, IN_TYPE).sequence(DELEGATE))
				.sequence(getValidIdentifiers(), LPAREN)
				.createNode(EShallowEntityType.METHOD, 0, -2).skipTo(RPAREN)
				.skipTo(SEMICOLON).endNode();

		createMethodRules();

		// properties
		typePatternInState(IN_TYPE)
				.subRecognizer(createExplicitInterfaceQualifierRecognizer(), 0,
						Integer.MAX_VALUE)
				.sequence(getValidIdentifiers(), LBRACE)
				.createNode(EShallowEntityType.ATTRIBUTE, "property", -2)
				.parseUntil(IN_TYPE).sequence(RBRACE).endNode();

		// events
		RecognizerBase<EGenericParserStates> eventRecognizer = inState(
				TOP_LEVEL, IN_MODULE, IN_TYPE).sequence(EVENT,
				getValidIdentifiers(), getValidIdentifiers()).createNode(
				EShallowEntityType.ATTRIBUTE, 0, -1);
		eventRecognizer.sequence(LBRACE).parseUntil(IN_TYPE).sequence(RBRACE)
				.endNode();
		eventRecognizer.skipTo(SEMICOLON).endNode();

		// attributes, e.g., fields (must be after method, as this would also
		// match methods)
		typePatternInState(IN_TYPE)
				.sequence(getValidIdentifiers())
				.createNode(EShallowEntityType.ATTRIBUTE, "attribute", -1)
				.skipToWithNesting(SEMICOLON, LBRACE, RBRACE,
						getSubExpressionRecognizer()).endNode();

		// static initializer, get/set for properties, add/remove in events
		inState(IN_TYPE)
				.sequence(EnumSet.of(STATIC, GET, SET, ADD, REMOVE), LBRACE)
				.createNode(EShallowEntityType.METHOD, 0).parseUntil(IN_METHOD)
				.sequence(RBRACE).endNode();
	}

	/**
	 * Creates the rules for all method-like constructs inside types.
	 */
	private void createMethodRules() {
		// indexers
		completeMethod(
				"indexer",
				EShallowEntityType.ATTRIBUTE,
				IN_TYPE,
				typePatternInState(IN_TYPE)
						.subRecognizer(
								createExplicitInterfaceQualifierRecognizer(),
								0, Integer.MAX_VALUE).markStart()
						.sequence(THIS, LBRACK).skipTo(RBRACK));

		// operator overloading
		completeMethod(
				"operator",
				EShallowEntityType.METHOD,
				IN_METHOD,
				typePatternInState(IN_TYPE)
						.sequence(OPERATOR)
						.markStart()
						.sequence(
								EnumSet.of(ETokenClass.OPERATOR,
										ETokenClass.KEYWORD), LPAREN)
						.skipTo(RPAREN));

		// methods
		completeMethod(
				"method",
				EShallowEntityType.METHOD,
				IN_METHOD,
				typePatternInState(IN_TYPE)
						.subRecognizer(
								createExplicitInterfaceQualifierRecognizer(),
								0, Integer.MAX_VALUE).markStart()
						.sequence(getValidIdentifiers()).skipNested(LT, GT)
						.sequence(LPAREN).skipTo(RPAREN));

		// constructor
		inState(IN_TYPE)
				.repeated(EnumSet.of(PRIVATE, PROTECTED, PUBLIC, INTERNAL))
				.markStart().sequence(getValidIdentifiers(), LPAREN)
				.skipTo(RPAREN).skipToWithNesting(LBRACE, LPAREN, RPAREN)
				.createNode(EShallowEntityType.METHOD, "constructor", 0)
				.parseUntil(IN_METHOD).sequence(RBRACE).endNode();
	}

	/**
	 * Creates a new recognizer that can match an explicit interface qualifier
	 * prefix for a method-like construct. This includes sequences of
	 * identifiers with dots, possibly intermixed with template arguments.
	 */
	private RecognizerBase<EGenericParserStates> createExplicitInterfaceQualifierRecognizer() {
		// remember the start of the recognizer chain (we can not use the
		// result of the method chain, as this would be the last recognizer)
		RecognizerBase<EGenericParserStates> result = emptyRecognizer();
		result.sequence(getValidIdentifiers()).skipNested(LT, GT).sequence(DOT);
		return result;
	}

	/**
	 * Completes a method-like construct. This begins with searching for the
	 * first semicolon or brace, i.e., the parameter list should already be
	 * skipped. This ends either in a complete method with a body, or with a
	 * semicolon and thus is just an abstract method.
	 */
	private void completeMethod(String name, EShallowEntityType nodeType,
			EGenericParserStates subParseState,
			RecognizerBase<EGenericParserStates> start) {
		RecognizerBase<EGenericParserStates> alternative = start
				.skipBefore(EnumSet.of(LBRACE, SEMICOLON));
		alternative.sequence(LBRACE).createNode(nodeType, name, 0)
				.parseUntil(subParseState).sequence(RBRACE).endNode();
		alternative.sequence(SEMICOLON)
				.createNode(nodeType, "abstract " + name, 0).endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected void createCaseRule() {
		super.createCaseRule();

		// C# also allows any kind of constant expression as a case label, e.g:
		// Foo.BAR + Foo.GOO << 12
		inState(IN_METHOD).markStart().sequence(CASE).skipTo(COLON)
				.createNode(EShallowEntityType.META, 0).endNode();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Also returns all contextual keywords, as they are valid identifiers in
	 * the language. See http://msdn.microsoft.com/en-us/library/x53a06bb.aspx
	 * for the full list.
	 */
	@Override
	protected EnumSet<ETokenType> getValidIdentifiers() {
		return EnumSet
				.of(IDENTIFIER, ADD, ALIAS, ASCENDING, ASYNC, AWAIT,
						DESCENDING, DYNAMIC, FROM, GET, GLOBAL, GROUP, INTO,
						JOIN, LET, ORDERBY, PARTIAL, REMOVE, SELECT, SET,
						VALUE, VAR, WHERE, YIELD);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getSimpleBlockKeywordsWithParentheses() {
		return EnumSet.of(WHILE, FOR, SWITCH, LOCK, USING, FIXED, FOREACH);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getSimpleBlockKeywordsWithoutParentheses() {
		return EnumSet.of(ELSE, FINALLY, CHECKED, UNCHECKED, UNSAFE);
	}

	/** {@inheritDoc} */
	@Override
	protected EnumSet<ETokenType> getStatementStartTokens() {
		return EnumSet.of(NEW, BREAK, CONTINUE, RETURN, ASSERT, CONST, GOTO,
				BASE, THROW, THIS, CHECKED, SIZEOF, STACKALLOC, TYPEOF, VALUE,
				YIELD, LPAREN, PLUSPLUS, MINUSMINUS);
	}

	/** {@inheritDoc} */
	@Override
	protected RecognizerBase<EGenericParserStates> typePattern(
			RecognizerBase<EGenericParserStates> currentState) {
		EnumSet<ETokenType> modifierKeywords = EnumSet.of(PRIVATE, PROTECTED,
				PUBLIC, INTERNAL, VIRTUAL, ABSTRACT, ASYNC, CONST, EXTERN,
				OVERRIDE, READONLY, UNSAFE, VOLATILE, NEW, STATIC);
		EnumSet<ETokenType> typeStart = EnumSet.of(VOID, BYTE, SHORT, INT,
				LONG, FLOAT, DOUBLE, CHAR, BOOL, STRING, OBJECT, DECIMAL,
				SBYTE, USHORT, UINT, OPERATOR, ULONG);
		typeStart.addAll(getValidIdentifiers());

		// we include "?" in the skipping section to deal with nullable types
		// and the comma for multi-dim arrays
		return currentState.repeated(modifierKeywords).sequence(typeStart)
				.skipNested(LT, GT)
				.skipAny(EnumSet.of(LBRACK, RBRACK, QUESTION, COMMA));
	}

	/** {@inheritDoc} */
	@Override
	protected void createSubExpressionRules() {
		// anonymous delegate methods
		inState(IN_EXPRESSION).sequence(DELEGATE, LPAREN)
				.createNode(EShallowEntityType.METHOD, "anonymous method")
				.skipToWithNesting(RPAREN, LPAREN, RPAREN).sequence(LBRACE)
				.parseUntil(IN_METHOD).sequence(RBRACE).endNode();

		// lambda expressions
		completeLambda(inState(IN_EXPRESSION).sequence(getValidIdentifiers()));
		completeLambda(inState(IN_EXPRESSION).sequence(LPAREN).skipTo(RPAREN));

		// additional rule for parsing lambda expressions (without braces). see
		// completeLambda() for details
		// the node start is moved one token to the right so the shallow
		// entities produced by this rule don't include the double arrow
		// (instead it will be included in the parent entity)
		inState(IN_EXPRESSION)
				.sequence(DOUBLE_ARROW)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.LAMBDA_EXPRESSION, null, 1)
				.skipBeforeWithNesting(
						EnumSet.of(RPAREN, SEMICOLON, RBRACE, COMMA), LPAREN,
						RPAREN).endNode();
	}

	/** Completes a rule for parsing lambda expressions. */
	private void completeLambda(RecognizerBase<EGenericParserStates> ruleStart) {
		RecognizerBase<EGenericParserStates> lambdaAlternative = ruleStart
				.createNode(EShallowEntityType.METHOD, "lambda");
		lambdaAlternative.sequence(DOUBLE_ARROW, LBRACE).parseUntil(IN_METHOD)
				.sequence(RBRACE).endNode();

		// we start parsing before the double arrow (=>) as this allows our
		// special statement rule to capture this case. This is required, as
		// this kind of expression is not terminated by a semicolon.
		lambdaAlternative.sequenceBefore(DOUBLE_ARROW).parseOnce(IN_EXPRESSION)
				.endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected RecognizerBase<EGenericParserStates> getSubExpressionRecognizer() {
		return new CsDelegateAndLambdaRecognizer();
	}

}
