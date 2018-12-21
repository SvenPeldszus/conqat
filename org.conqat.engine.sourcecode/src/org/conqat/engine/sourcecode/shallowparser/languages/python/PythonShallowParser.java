/*-------------------------------------------------------------------------+
|                                                                          |
Copyright 2005-2011 the ConQAT Project                                   |
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
package org.conqat.engine.sourcecode.shallowparser.languages.python;

import static org.conqat.engine.sourcecode.shallowparser.languages.python.PythonShallowParser.EPythonParserStates.ANY;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.DEDENT;
import static org.conqat.lib.scanner.ETokenType.DEF;
import static org.conqat.lib.scanner.ETokenType.ELIF;
import static org.conqat.lib.scanner.ETokenType.ELSE;
import static org.conqat.lib.scanner.ETokenType.EOL;
import static org.conqat.lib.scanner.ETokenType.EXCEPT;
import static org.conqat.lib.scanner.ETokenType.FINALLY;
import static org.conqat.lib.scanner.ETokenType.FOR;
import static org.conqat.lib.scanner.ETokenType.FROM;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.IMPORT;
import static org.conqat.lib.scanner.ETokenType.INDENT;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.TRY;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.engine.sourcecode.shallowparser.languages.python.PythonShallowParser.EPythonParserStates;
import org.conqat.lib.scanner.IToken;

/**
 * Shallow parser for Python.
 *
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: 1E117B272044D171DEC64B4E93809B6C
 */
public class PythonShallowParser extends ShallowParserBase<EPythonParserStates> {

	/** The states used in this parser. */
	public static enum EPythonParserStates {

		/** Single state, as any construct can occur at any place. */
		ANY
	}

	/** Constructor. */
	public PythonShallowParser() {
		super(EPythonParserStates.class, EPythonParserStates.ANY);

		createErrorRules();
		createImportRules();
		createClassRules();
		createFunctionRules();
		createStatementRules();
	}

	/** Create rules for handling error handling. */
	private void createErrorRules() {
		// unmatched indent/dedent: no endNode to keep the node incomplete
		inState(ANY).sequence(INDENT).createNode(EShallowEntityType.META,
				"Unmatched indent");
		inState(ANY).sequence(DEDENT).createNode(EShallowEntityType.META,
				"Unmatched dedent");
	}

	/** Creates parsing rules for classes. */
	private void createImportRules() {
		inAnyState().sequence(EnumSet.of(IMPORT, FROM))
				.createNode(EShallowEntityType.META, 0).skipTo(EOL).endNode();
	}

	/** Creates parsing rules for classes. */
	private void createClassRules() {
		RecognizerBase<EPythonParserStates> classAlternative = inState(ANY)
				.sequence(CLASS, IDENTIFIER).skipNested(LPAREN, RPAREN)
				.sequence(COLON)
				.createNode(EShallowEntityType.TYPE, "class", 1);
		addBlockClosingAlternatives(classAlternative);
	}

	/** Creates parsing rules for functions. */
	private void createFunctionRules() {
		RecognizerBase<EPythonParserStates> functionAlternative = inState(ANY)
				.sequence(DEF, IDENTIFIER).skipNested(LPAREN, RPAREN)
				.sequence(COLON)
				.createNode(EShallowEntityType.METHOD, "method", 1);
		addBlockClosingAlternatives(functionAlternative);
	}

	/** Creates parsing rules for statements. */
	private void createStatementRules() {
		// empty statement
		inState(ANY)
				.sequence(SEMICOLON)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.EMPTY_STATEMENT).endNode();

		// block statements
		RecognizerBase<EPythonParserStates> ifAlternative = inState(ANY)
				.sequence(
						EnumSet.of(IF, ELIF, ELSE, TRY, EXCEPT, FINALLY, WHILE,
								FOR))
				.createNode(EShallowEntityType.STATEMENT, 0)
				.skipToWithNesting(COLON, LBRACK, RBRACK);
		addBlockClosingAlternatives(ifAlternative);

		// remove any isolated EOLs
		inState(ANY).sequence(EOL);

		// simple statement
		inState(ANY)
				.sequenceBefore(EnumSet.complementOf(EnumSet.of(DEF, CLASS)))
				.subRecognizer(new PythonSimpleStatementRecognizer(), 1, 1)
				.endNode();
	}

	/**
	 * Adds two different rules for closing a block:
	 * <ul>
	 * <li>Closing a block by finding a dedent</li>
	 * <li>Single line that ends with EOL, typically this means multiple
	 * statements on one line</li>
	 * </ul>
	 */
	private void addBlockClosingAlternatives(
			RecognizerBase<EPythonParserStates> matchingAlternative) {
		matchingAlternative.sequence(EOL, INDENT).parseUntil(ANY)
				.sequence(DEDENT).endNode();
		matchingAlternative.parseUntil(ANY).sequence(EOL).endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFilteredToken(IToken token, IToken previousToken) {
		if (super.isFilteredToken(token, previousToken)) {
			return true;
		}
		// Don't allow double EOLs
		return previousToken != null && previousToken.getType() == EOL
				&& token.getType() == EOL;
	}

}
