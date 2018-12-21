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
package org.conqat.engine.sourcecode.shallowparser.languages.magik;

import static org.conqat.engine.sourcecode.shallowparser.languages.magik.MagikShallowParser.EMagikParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.ABSTRACT;
import static org.conqat.lib.scanner.ETokenType.ASSIGN;
import static org.conqat.lib.scanner.ETokenType.AT_OPERATOR;
import static org.conqat.lib.scanner.ETokenType.BLOCK;
import static org.conqat.lib.scanner.ETokenType.CATCH;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.DYNAMIC;
import static org.conqat.lib.scanner.ETokenType.ELIF;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.ENDCATCH;
import static org.conqat.lib.scanner.ETokenType.ENDLOOP;
import static org.conqat.lib.scanner.ETokenType.ENDMETHOD;
import static org.conqat.lib.scanner.ETokenType.ENDTRY;
import static org.conqat.lib.scanner.ETokenType.END_BLOCK;
import static org.conqat.lib.scanner.ETokenType.END_IF;
import static org.conqat.lib.scanner.ETokenType.END_LOCK;
import static org.conqat.lib.scanner.ETokenType.END_PROC;
import static org.conqat.lib.scanner.ETokenType.END_PROTECT;
import static org.conqat.lib.scanner.ETokenType.EOL;
import static org.conqat.lib.scanner.ETokenType.FINALLY;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.GLOBAL;
import static org.conqat.lib.scanner.ETokenType.HANDLE;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.ITERATE;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LOCAL;
import static org.conqat.lib.scanner.ETokenType.LOCK;
import static org.conqat.lib.scanner.ETokenType.LOCKING;
import static org.conqat.lib.scanner.ETokenType.LOOP;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.METHOD;
import static org.conqat.lib.scanner.ETokenType.OVER;
import static org.conqat.lib.scanner.ETokenType.PACKAGE;
import static org.conqat.lib.scanner.ETokenType.PRAGMA;
import static org.conqat.lib.scanner.ETokenType.PRIVATE;
import static org.conqat.lib.scanner.ETokenType.PROC;
import static org.conqat.lib.scanner.ETokenType.PROTECT;
import static org.conqat.lib.scanner.ETokenType.PROTECTED;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SELF;
import static org.conqat.lib.scanner.ETokenType.TERMINATE;
import static org.conqat.lib.scanner.ETokenType.THEN;
import static org.conqat.lib.scanner.ETokenType.TRY;
import static org.conqat.lib.scanner.ETokenType.WHEN;
import static org.conqat.lib.scanner.ETokenType.WITH;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.magik.MagikShallowParser.EMagikParserStates;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.scanner.IToken;

/**
 * Shallow parser for the language <a
 * href="http://en.wikipedia.org/wiki/Magik_%28programming_language%29"
 * >Magik</a>.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49551 $
 * @ConQAT.Rating GREEN Hash: BFDEC02A1B9C20C20C8135A26860C8FE
 */
public class MagikShallowParser extends ShallowParserBase<EMagikParserStates> {

	/** The states used in this parser. */
	public static enum EMagikParserStates {

		/** Single state, as any construct can occur at any place. */
		ANY
	}

	/** Constructor. */
	public MagikShallowParser() {
		super(EMagikParserStates.class, EMagikParserStates.ANY);

		createMetaRules();
		createMethodRules();
		createProcedureAndHandlerRules();
		createStatementRules();
	}

	/** Creates parsing rules for meta elements. */
	private void createMetaRules() {
		inState(ANY).sequence(PACKAGE, IDENTIFIER)
				.createNode(EShallowEntityType.META, 0, 1).endNode();

		inState(ANY).sequence(PRAGMA).createNode(EShallowEntityType.META, 0)
				.skipNested(LPAREN, RPAREN).endNode();
	}

	/** Create rules for parsing methods. */
	private void createMethodRules() {
		RecognizerBase<EMagikParserStates> methodAlternative = inState(ANY)
				.repeated(EnumSet.of(PRIVATE, ITERATE, ABSTRACT)).markStart()
				.sequence(METHOD, IDENTIFIER, DOT, IDENTIFIER);

		// assigner (<<)
		methodAlternative
				.sequence(ASSIGN, IDENTIFIER)
				.createNode(EShallowEntityType.METHOD, "method",
						new Region(1, -2)).parseUntil(ANY).sequence(ENDMETHOD)
				.endNode();
		// method with (); we include the "(" in the name of the method to avoid
		// name clashes
		methodAlternative
				.sequence(LPAREN)
				.createNode(EShallowEntityType.METHOD, "method",
						new Region(1, -1)).skipTo(RPAREN).parseUntil(ANY)
				.sequence(ENDMETHOD).endNode();
		// method without ()
		methodAlternative
				.createNode(EShallowEntityType.METHOD, "method",
						new Region(1, -1)).parseUntil(ANY).sequence(ENDMETHOD)
				.endNode();

		inState(ANY)
				.repeated(EnumSet.of(PRIVATE, ITERATE, ABSTRACT))
				.markStart()
				.sequence(METHOD, IDENTIFIER, LBRACK)
				.createNode(EShallowEntityType.METHOD, "method",
						new Region(1, 3)).skipTo(RBRACK).parseUntil(ANY)
				.sequence(ENDMETHOD).endNode();
	}

	/** Creates rules for parsing procedures and handlers. */
	private void createProcedureAndHandlerRules() {
		// procedures
		completeProcedure(inState(ANY).sequence(PROC).createNode(
				EShallowEntityType.METHOD, "anonymous procedure"));
		completeProcedure(inState(ANY)
				.repeated(EnumSet.of(GLOBAL, DYNAMIC, LOCAL)).markStart()
				.sequence(IDENTIFIER, ASSIGN, PROC)
				.createNode(EShallowEntityType.METHOD, "procedure", 0));

		// handlers
		completeProcedure(inState(ANY).sequence(HANDLE).markStart()
				.skipTo(WITH).sequence(PROC)
				.createNode(EShallowEntityType.METHOD, "handler", 0));

	}

	/** Completes a created procedure node. */
	private void completeProcedure(
			RecognizerBase<EMagikParserStates> procedureStart) {
		procedureStart.optional(AT_OPERATOR, IDENTIFIER)
				.skipNested(LPAREN, RPAREN).parseUntil(ANY).sequence(END_PROC)
				.endNode();
	}

	/** Create rules for statements. */
	private void createStatementRules() {
		createIfElseRules();
		createLoopRules();
		createBlockRules();

		// simple statement must be the very last
		inState(ANY).subRecognizer(new MagikSimpleStatementRecognizer(), 1, 1)
				.endNode();
	}

	/**
	 * Creats the rules for parsing blocks and block-like constructs (try, lock,
	 * etc.).
	 */
	private void createBlockRules() {
		// general blocks
		inState(ANY).sequence(BLOCK)
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequence(END_BLOCK).endNode();

		// exception handling (try/when)
		inState(ANY).sequence(TRY).createNode(EShallowEntityType.STATEMENT, 0)
				.parseUntil(ANY).sequenceBefore(WHEN).endNodeWithContinuation();
		RecognizerBase<EMagikParserStates> whenAlternative = inState(ANY)
				.sequence(WHEN, IDENTIFIER)
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequenceBefore(EnumSet.of(WHEN, ENDTRY));
		whenAlternative.sequence(ENDTRY).endNode();
		whenAlternative.endNodeWithContinuation();

		// catch blocks
		inState(ANY).sequence(CATCH).optional(COLON)
				.optional(EnumSet.of(IDENTIFIER, SELF))
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequence(ENDCATCH).endNode();

		// protect
		inState(ANY).sequence(PROTECT)
				.optional(LOCKING, EnumSet.of(IDENTIFIER, SELF))
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequenceBefore(PROTECTED).endNodeWithContinuation();
		inState(ANY).sequence(PROTECTED)
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequence(END_PROTECT).endNode();

		// lock
		inState(ANY).sequence(LOCK).optional(COLON)
				.optional(EnumSet.of(IDENTIFIER, SELF))
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequence(END_LOCK).endNode();
	}

	/** Creates the rules for parsing loops. */
	private void createLoopRules() {
		completeLoop(inState(ANY).sequence(LOOP));
		completeLoop(inState(ANY).sequence(EnumSet.of(FOR, OVER)).skipTo(LOOP));
		inState(ANY).sequence(FINALLY)
				.createNode(EShallowEntityType.STATEMENT, 0).parseUntil(ANY)
				.sequence(ENDLOOP).endNode();
	}

	/** Creates the rules for parsing if/else statements. */
	private void createIfElseRules() {
		RecognizerBase<EMagikParserStates> ifAlternative = inState(ANY)
				.sequence(EnumSet.of(IF, ELIF))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(THEN)
				.parseUntil(ANY).sequenceBefore(EnumSet.of(ELIF, ELSE, END_IF));
		ifAlternative.sequence(END_IF).endNode();
		ifAlternative.endNodeWithContinuation();

		inState(ANY).sequence(ELSE).createNode(EShallowEntityType.STATEMENT, 0)
				.parseUntil(ANY).sequence(END_IF).endNode();
	}

	/** Completes a loop construct, handling the finally case. */
	private void completeLoop(RecognizerBase<EMagikParserStates> loopStart) {
		RecognizerBase<EMagikParserStates> loopEndAlternative = loopStart
				.createNode(EShallowEntityType.STATEMENT, 0)
				.optional(AT_OPERATOR, IDENTIFIER).parseUntil(ANY)
				.sequenceBefore(EnumSet.of(ENDLOOP, FINALLY));
		loopEndAlternative.sequence(ENDLOOP).endNode();
		loopEndAlternative.endNodeWithContinuation();
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFilteredToken(IToken token, IToken previousToken) {
		return super.isFilteredToken(token, previousToken)
				|| token.getType() == TERMINATE || token.getType() == EOL;
	}

}
