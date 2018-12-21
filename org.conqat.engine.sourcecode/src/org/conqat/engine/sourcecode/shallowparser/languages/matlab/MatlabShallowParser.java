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
package org.conqat.engine.sourcecode.shallowparser.languages.matlab;

import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_CLASSDEF;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_ENUMERATION;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_EVENTS;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_METHODS;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_PROPERTIES;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.IN_SWITCH;
import static org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates.TOP_LEVEL;
import static org.conqat.lib.scanner.ETokenType.AND;
import static org.conqat.lib.scanner.ETokenType.BREAK;
import static org.conqat.lib.scanner.ETokenType.CASE;
import static org.conqat.lib.scanner.ETokenType.CATCH;
import static org.conqat.lib.scanner.ETokenType.CLASSDEF;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.CONTINUE;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ELSEIF;
import static org.conqat.lib.scanner.ETokenType.END;
import static org.conqat.lib.scanner.ETokenType.ENUMERATION;
import static org.conqat.lib.scanner.ETokenType.EOF;
import static org.conqat.lib.scanner.ETokenType.EOL;
import static org.conqat.lib.scanner.ETokenType.EQ;
import static org.conqat.lib.scanner.ETokenType.EVENTS;
import static org.conqat.lib.scanner.ETokenType.EXCLAMATION;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.FUNCTION;
import static org.conqat.lib.scanner.ETokenType.GLOBAL;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.METHODS;
import static org.conqat.lib.scanner.ETokenType.OTHERWISE;
import static org.conqat.lib.scanner.ETokenType.PARFOR;
import static org.conqat.lib.scanner.ETokenType.PERSISTENT;
import static org.conqat.lib.scanner.ETokenType.PROPERTIES;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RETURN;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.SWITCH;
import static org.conqat.lib.scanner.ETokenType.TRY;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.matlab.MatlabShallowParser.EMatlabParserStates;
import org.conqat.lib.scanner.ETokenType;

/**
 * A shallow parser for Matlab.
 * 
 * This parser currently recognizes class definitions (with events, enumeration,
 * methods and properties). Within functions, control structures and simple
 * statements can be parsed. Nested methods and shell escapes are supported.
 * 
 * @author $Author: kupka $
 * @version $Rev: 51796 $
 * @ConQAT.Rating YELLOW Hash: 0A6182BE67CA0B085F2CFCE45564122B
 */
// TODO (BH): NOte to self: just a shallow review. perform full review in next
// round
public class MatlabShallowParser extends ShallowParserBase<EMatlabParserStates> {

	/** All possible states of a MatlabShallowParser. */
	public static enum EMatlabParserStates {
		/** Top-level state. */
		TOP_LEVEL,

		/** Inside a class definition. */
		IN_CLASSDEF,

		/** Inside a properties section. */
		IN_PROPERTIES,

		/** Inside a events section. */
		IN_EVENTS,

		/** Inside a enumeration section. */
		IN_ENUMERATION,

		/** Inside a methods section. */
		IN_METHODS,

		/** Inside a method's body. */
		IN_METHOD,

		/** Inside a switch block. */
		IN_SWITCH
	}

	/** All tokens that are valid statement separators. */
	private static final EnumSet<ETokenType> STATEMENT_SEPARATORS = EnumSet.of(
			EOL, SEMICOLON);

	/** All states in which statements can be parsed. */
	private static final EMatlabParserStates[] STATEMENT_STATES = { TOP_LEVEL,
			IN_METHOD };

	/** Constructor. */
	public MatlabShallowParser() {
		super(EMatlabParserStates.class, TOP_LEVEL);
		createTopLevelRules();
		createInClassDefRules();
		createInPropertiesRules();
		createInEventsRules();
		createInEnumerationRules();
		createInMethodsRules();
		createStatementRules();
	}

	/** Creates rules for the top-level state. */
	private void createTopLevelRules() {
		// class definition
		inState(TOP_LEVEL).sequence(CLASSDEF)
				.skipToWithNesting(IDENTIFIER, LPAREN, RPAREN)
				.createNode(EShallowEntityType.TYPE, "class", -1)
				.optional(LT, IDENTIFIER).repeated(AND, IDENTIFIER)
				.sequence(EOL).parseUntil(IN_CLASSDEF).sequence(END).endNode();
	}

	/** Creates rules for parsing class definitions. */
	private void createInClassDefRules() {
		// properties section
		createInClassDefSectionRule(PROPERTIES, IN_PROPERTIES);

		// methods section
		createInClassDefSectionRule(METHODS, IN_METHODS);

		// events section
		createInClassDefSectionRule(EVENTS, IN_EVENTS);

		// enumeration section
		inState(IN_CLASSDEF).sequence(ENUMERATION, EOL)
				.createNode(EShallowEntityType.META, 0)
				.parseUntil(IN_ENUMERATION).sequence(END).endNode();
	}

	/**
	 * Creates a rule for parsing a section within a class definition. The
	 * section must start with the given start token. The section's content is
	 * parsed in the given sub-state.
	 */
	private void createInClassDefSectionRule(ETokenType startTokenType,
			EMatlabParserStates subState) {
		inState(IN_CLASSDEF).sequence(startTokenType)
				.skipToWithNesting(EOL, LPAREN, RPAREN)
				.createNode(EShallowEntityType.META, 0).parseUntil(subState)
				.sequence(END).endNode();
	}

	/** Creates rules for parsing class properties. */
	private void createInPropertiesRules() {
		inState(IN_PROPERTIES).sequence(IDENTIFIER)
				.createNode(EShallowEntityType.ATTRIBUTE, "attribute", 0)
				.skipTo(STATEMENT_SEPARATORS).endNode();
	}

	/** Creates rules for parsing class events. */
	private void createInEventsRules() {
		inState(IN_EVENTS).sequence(IDENTIFIER)
				.createNode(EShallowEntityType.ATTRIBUTE, 0).sequence(EOL)
				.endNode();
	}

	/** Creates rules for parsing enumerations. */
	private void createInEnumerationRules() {
		inState(IN_ENUMERATION).sequence(IDENTIFIER)
				.createNode(EShallowEntityType.ATTRIBUTE, "enum-literal", 0)
				.skipToWithNesting(EnumSet.of(COMMA, EOL), LPAREN, RPAREN)
				.endNode();
	}

	/** Creates rules for parsing function definitions/declarations. */
	private void createInMethodsRules() {
		// recognizer for the beginning of function definitions
		// function definitions can also appear in top-level state or within
		// another function's body
		RecognizerBase<EMatlabParserStates> functionAlternative = inState(
				TOP_LEVEL, IN_METHOD, IN_METHODS).sequence(FUNCTION);

		// function with one return value
		continueMethodHeadRule(functionAlternative.sequence(IDENTIFIER, EQ),
				true);

		// function with no return value
		continueMethodHeadRule(functionAlternative, true);

		// function with multiple return values
		continueMethodHeadRule(
				functionAlternative.skipToWithNesting(EQ, LBRACK, RBRACK), true);

		// function declaration
		continueMethodHeadRule(inState(IN_METHODS), false);
	}

	/** Appends rules for parsing a method's head to the given recognizer. */
	private void continueMethodHeadRule(
			RecognizerBase<EMatlabParserStates> functionRecognizer,
			boolean isDefinition) {
		String subtype = "function declaration";
		if (isDefinition) {
			subtype = "function";
		}

		functionRecognizer = functionRecognizer.sequence(IDENTIFIER)
				.createNode(EShallowEntityType.METHOD, subtype, -1)
				.skipToWithNesting(EOL, LPAREN, RPAREN);
		// if it is a function definition, we have to parse the function's body,
		// as well
		if (isDefinition) {
			// TODO (AK): the EOF token must be provided to this parser,
			// probably by overriding one of the parse methods and manually
			// appending an EOF token, but this feels like a dirty hack
			functionRecognizer = functionRecognizer.parseUntil(IN_METHOD)
					.sequence(EnumSet.of(END, EOF));
		}
		functionRecognizer.endNode();
	}

	/** Create rules for parsing all kinds of statements within function bodies. */
	private void createStatementRules() {
		// create rule for if/elseif/else
		createBlockRulesWithContinuation(EnumSet.of(IF, ELSEIF, ELSE),
				EnumSet.of(ELSEIF, ELSE));

		// create rule try/catch
		createBlockRulesWithContinuation(EnumSet.of(TRY, CATCH),
				EnumSet.of(CATCH));

		createLoopRules();
		createSwitchCaseRules();
		createShellEscapeRules();
		createSimpleStatementRule();
	}

	/**
	 * Creates a rules for parsing statements that begin with one of the given
	 * start tokens, followed by a block. The statement can be continued if one
	 * of the given continuation tokens is encountered after the block.
	 */
	private void createBlockRulesWithContinuation(
			EnumSet<ETokenType> startTokens,
			EnumSet<ETokenType> continuationTokens) {
		RecognizerBase<EMatlabParserStates> alternative = inState(
				STATEMENT_STATES).sequence(startTokens)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(EOL)
				.parseUntil(IN_METHOD);

		alternative.sequence(END).endNode();

		alternative.sequenceBefore(continuationTokens)
				.endNodeWithContinuation();
	}

	/** Creates rules for parsing for/parfor and while loops. */
	private void createLoopRules() {
		// create rule for for/parfor/while loop
		inState(STATEMENT_STATES).sequence(EnumSet.of(FOR, PARFOR, WHILE))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(EOL)
				.parseUntil(IN_METHOD).sequence(END).endNode();
	}

	/** Creates rules for parsing switch/case structures. */
	private void createSwitchCaseRules() {
		inState(STATEMENT_STATES).sequence(SWITCH)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(EOL)
				.parseUntil(IN_SWITCH).sequence(END).endNode();

		inState(IN_SWITCH).sequence(EnumSet.of(CASE, OTHERWISE))
				.createNode(EShallowEntityType.META, 0).skipTo(EOL)
				.parseUntil(IN_METHOD)
				.sequenceBefore(EnumSet.of(CASE, OTHERWISE, END)).endNode();
	}

	/** Returns a set of token types, that can start a simple statement. */
	private EnumSet<ETokenType> getSimpleStatementTokenTypes() {
		return EnumSet.of(IDENTIFIER, BREAK, CONTINUE, RETURN, LBRACK,
				PERSISTENT, GLOBAL);
	}

	/** Creates rules for parsing shell escapes. */
	private void createShellEscapeRules() {
		inState(STATEMENT_STATES).sequence(EXCLAMATION)
				.createNode(EShallowEntityType.STATEMENT, "shell-escape")
				.skipTo(EOL).endNode();
	}

	/** Creates rules for parsing simple statements. */
	private void createSimpleStatementRule() {
		inState(STATEMENT_STATES)
				.sequence(getSimpleStatementTokenTypes())
				.createNode(EShallowEntityType.STATEMENT, "simple-statement", 0)
				.skipTo(STATEMENT_SEPARATORS).endNode();
	}
}
