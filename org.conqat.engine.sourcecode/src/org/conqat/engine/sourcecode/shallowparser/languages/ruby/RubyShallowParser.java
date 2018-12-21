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
package org.conqat.engine.sourcecode.shallowparser.languages.ruby;

import static org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType.METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.ruby.RubyShallowParser.ERubyParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.BEGIN;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.DEF;
import static org.conqat.lib.scanner.ETokenType.DO;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ELSIF;
import static org.conqat.lib.scanner.ETokenType.END;
import static org.conqat.lib.scanner.ETokenType.ENSURE;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.INTERPOLATIONEND;
import static org.conqat.lib.scanner.ETokenType.INTERPOLATIONSTART;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LSHIFT;
import static org.conqat.lib.scanner.ETokenType.MODULE;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RESCUE;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SELF;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.STRING_LITERAL;
import static org.conqat.lib.scanner.ETokenType.UNLESS;
import static org.conqat.lib.scanner.ETokenType.UNTIL;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.ruby.RubyShallowParser.ERubyParserStates;
import org.conqat.lib.commons.region.Region;

/**
 * Shallow parser for JavaScript. The parser is aware of Google closure and
 * supports special handling of the provide, require and inherits statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating RED Hash: 9235BF942E1E1D0BFD5B2C16067DB9E9
 */
public class RubyShallowParser extends ShallowParserBase<ERubyParserStates> {

	/** The states used in this parser. */
	public static enum ERubyParserStates {

		/** Single state, as any construct can occur at any place. */
		ANY
	}

	/** Constructor. */
	public RubyShallowParser() {
		super(ERubyParserStates.class, ERubyParserStates.ANY);

		createStringLiteralRules();
		createClassRules();
		createFunctionRules();
		createStatementRules();
	}

	/**
	 * Stitches together string literals with interpolations.
	 */
	private void createStringLiteralRules() {
		inState(ANY).sequence(STRING_LITERAL)
				.skipNested(INTERPOLATIONSTART, INTERPOLATIONEND)
				.optional(STRING_LITERAL);
	}

	/** Creates parsing rules for classes. */
	private void createClassRules() {
		// class << self
		inState(ANY).sequence(CLASS, LSHIFT, SELF).optional(SEMICOLON)
				.createNode(EShallowEntityType.META, "static declarations")
				.parseUntil(ANY).sequence(END).endNode();
		// class
		inState(ANY).sequence(CLASS, IDENTIFIER).optional(SEMICOLON)
				.createNode(EShallowEntityType.TYPE, "class", 1)
				.parseUntil(ANY).sequence(END).endNode();
		// module
		inState(ANY).sequence(MODULE, IDENTIFIER).optional(SEMICOLON)
				.createNode(EShallowEntityType.TYPE, "module", 1)
				.parseUntil(ANY).sequence(END).endNode();
	}

	/** Creates parsing rules for functions. */
	private void createFunctionRules() {
		// named function/method
		inState(ANY).sequence(DEF, IDENTIFIER).optional(SEMICOLON)
				.createNode(EShallowEntityType.METHOD, "method", 1)
				.skipNested(LPAREN, RPAREN).parseUntil(ANY).sequence(END)
				.endNode();
		// static function/method
		inState(ANY)
				.sequence(DEF, SELF, DOT, IDENTIFIER)
				.optional(SEMICOLON)
				.createNode(EShallowEntityType.METHOD, "method",
						new Region(1, 3)).skipNested(LPAREN, RPAREN)
				.parseUntil(ANY).sequence(END).endNode();
	}

	/** Creates parsing rules for statements. */
	private void createStatementRules() {
		// empty statement
		inState(ANY).sequence(SEMICOLON)
				.createNode(EShallowEntityType.STATEMENT, SubTypeNames.EMPTY_STATEMENT)
				.endNode();

		// blocks
		inState(ANY).sequence(DO).createNode(METHOD, "block").parseUntil(ANY)
				.sequence(END).endNode();
		inState(ANY).sequence(LBRACE).createNode(METHOD, "block")
				.parseUntil(ANY).sequence(RBRACE).endNode();

		// if statements
		RecognizerBase<ERubyParserStates> ifAlternative = inState(ANY)
				.sequence(EnumSet.of(IF, ELSIF, UNLESS))
				.createNode(EShallowEntityType.STATEMENT, 0)
				.subRecognizer(new RubyBlockStartRecognizer(), 1, 1)
				.parseUntil(ANY);
		appendTrailingConditionRecognizer(ifAlternative.sequence(END))
				.endNode();
		ifAlternative.sequenceBefore(EnumSet.of(ELSE, ELSIF))
				.endNodeWithContinuation();
		appendTrailingConditionRecognizer(
				inState(ANY).sequence(ELSE)
						.createNode(EShallowEntityType.STATEMENT, SubTypeNames.ELSE)
						.parseUntil(ANY).sequence(END)).endNode();

		// while/for/until statements
		appendTrailingConditionRecognizer(
				inState(ANY).sequence(EnumSet.of(WHILE, UNTIL, FOR))
						.createNode(EShallowEntityType.STATEMENT, 0)
						.subRecognizer(new RubyBlockStartRecognizer(), 1, 1)
						.parseUntil(ANY).sequence(END)).endNode();

		// begin/end statements
		RecognizerBase<ERubyParserStates> beginAlternative = inState(ANY)
				.sequence(EnumSet.of(BEGIN))
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY);

		appendTrailingConditionRecognizer(beginAlternative.sequence(END))
				.endNode();
		beginAlternative.sequenceBefore(EnumSet.of(RESCUE, ENSURE))
				.endNodeWithContinuation();

		appendTrailingConditionRecognizer(
				inState(ANY).sequence(ENSURE)
						.createNode(EShallowEntityType.STATEMENT, 0)
						.parseUntil(ANY).sequence(END)).endNode();

		RecognizerBase<ERubyParserStates> rescueAlternative = inState(ANY)
				.sequenceBefore(RESCUE)
				.createNode(EShallowEntityType.STATEMENT, 0)
				.subRecognizer(new RubyBlockStartRecognizer(), 1, 1)
				.parseUntil(ANY);

		appendTrailingConditionRecognizer(rescueAlternative.sequence(END))
				.endNode();
		rescueAlternative.sequenceBefore(EnumSet.of(RESCUE, ENSURE))
				.endNodeWithContinuation();

		// TODO case/when statements

		// simple statement
		inState(ANY)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.SIMPLE_STATEMENT, 0)
				.subRecognizer(new RubySimpleStatementRecognizer(), 1, 1)
				.endNode();
	}

	/**
	 * @return
	 */
	private RecognizerBase<ERubyParserStates> appendTrailingConditionRecognizer(
			RecognizerBase<ERubyParserStates> recognizer) {
		return recognizer.subRecognizer(
				emptyRecognizer().sequence(
						EnumSet.of(FOR, IF, UNLESS, WHILE, UNTIL))
						.subRecognizer(new RubyTrailingConditionRecognizer(),
								1, 1), 0, Integer.MAX_VALUE);
	}
}
