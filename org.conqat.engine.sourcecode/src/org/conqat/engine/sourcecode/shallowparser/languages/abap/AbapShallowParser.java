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
package org.conqat.engine.sourcecode.shallowparser.languages.abap;

import static org.conqat.engine.sourcecode.shallowparser.languages.abap.AbapShallowParser.EAbapParserStates.DECLARATIONS;
import static org.conqat.engine.sourcecode.shallowparser.languages.abap.AbapShallowParser.EAbapParserStates.STATEMENTS;
import static org.conqat.engine.sourcecode.shallowparser.languages.abap.AbapShallowParser.EAbapParserStates.TOPLEVEL;
import static org.conqat.lib.scanner.ETokenType.*;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ParserState;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserBase;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Shallow parser for ABAP. The following links are useful for writing the
 * parser:
 * <ul>
 * <li><a href="http://help.sap.com/abapdocu_702/en/">ABAP Keyword
 * Documentation</a></li>
 * <li><a
 * href="http://help.sap.com/abapdocu_702/en/abenabap_statements_overview.htm"
 * >ABAP Statements Overview</a></li>
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51536 $
 * @ConQAT.Rating GREEN Hash: 5613669B22DC2CAA44136D74238A586B
 */
public class AbapShallowParser extends
		ShallowParserBase<AbapShallowParser.EAbapParserStates> {

	/** Tokens that can introduce a simple statement. */
	private static final EnumSet<ETokenType> SIMPLE_STATEMENT_START_TOKENS = EnumSet
			.of(ADD, ADD_CORRESPONDING, APPEND, ASSERT, ASSIGN,
					AUTHORITY_CHECK, BACK, BREAK_POINT, CALL, CHECK, CLEAR,
					CLOSE, COLLECT, COMMIT, COMMUNICATION, COMPUTE,
					CONCATENATE, CONDENSE, CONSTANTS, CONTEXTS, CONTINUE,
					CONTROLS, CONVERT, CREATE, DATA, DELETE, DEMAND, DESCRIBE,
					DETAIL, DIVIDE, DIVIDE_CORRESPONDING, EDITOR_CALL,
					ENHANCEMENT_POINT, EXISTS, EXIT, EXPORT, EXTRACT, FETCH,
					FIELDS, FIND, FORMAT, FREE, GENERATE, GET, HIDE,
					IDENTIFIER, IMPORT, INCLUDE, INFOTYPES, INPUT, INSERT,
					LEAVE, LOAD, LOCAL, LOG_POINT, MAXIMUM, MESSAGE, MINIMUM,
					MODIFY, MODULE, MOVE, MOVE_CORRESPONDING,
					MULTIPLY_CORRESPONDING, MULTIPLY, NAME, NEW_LINE, NEW_PAGE,
					NEW_SECTION, OPEN, OVERLAY, PACK, PACKAGE, PERFORM,
					POSITION, PRINT_CONTROL, PUT, RAISE, RANGES, READ, REFRESH,
					REJECT, REPLACE, RESERVE, RESUME, RETURN, ROLLBACK, SCROLL,
					SEARCH, SET, SHIFT, SKIP, SORT, SPLIT, STOP, SUBMIT,
					SUBTRACT, SUBTRACT_CORRESPONDING, SUM, SUMMARY, SUMMING,
					SUPPLY, SUPPRESS, SYNTAX_CHECK, TRANSFER, TRANSLATE,
					TRUNCATE, TYPES, ULINE, UNASSIGN, UNPACK, UPDATE, WAIT,
					WINDOW, WRITE);

	/** The states used in this parser. */
	public static enum EAbapParserStates {

		/**
		 * Top level state used for parsing constructs that are not nested in
		 * other constructs.
		 */
		TOPLEVEL,

		/**
		 * A state to recognize declarations within classes. As many constructs
		 * are allowed both top-level and in declarations, many rules are
		 * registered for both.
		 */
		DECLARATIONS,

		/** A state to recognize statements, i.e. plain code in functions, etc. */
		STATEMENTS
	}

	/** Constructor. */
	public AbapShallowParser() {
		super(EAbapParserStates.class, TOPLEVEL);

		createMetaRules();
		createTopLevelRules();
		createTypeRules();
		createMethodAndAttributeRules();
		createStatementRules();

		inAnyState()
				.sequence(DOT)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.EMPTY_STATEMENT).endNode();
	}

	/** Rules for parsing elements that are only expected top-level. */
	private void createTopLevelRules() {

		// set of keywords that start an event block (without keywords that
		// require a preceeding "at")
		EnumSet<ETokenType> eventBlocks = EnumSet.of(INITIALIZATION,
				START_OF_SELECTION, END_OF_SELECTION, TOP_OF_PAGE, END_OF_PAGE,
				LOAD_OF_PROGRAM);

		// set of keywords that end an event block (possibly indicating the
		// start of the next one)
		EnumSet<ETokenType> eventBlocksEnd = EnumSet.of(AT, FORM, CLASS,
				INTERFACE);
		eventBlocksEnd.addAll(eventBlocks);

		// since the report is not really a method, its statements are still
		// parsed in the toplevel scope, not the statement scope
		RecognizerBase<EAbapParserStates> reportRecognizer = inState(TOPLEVEL)
				.sequence(REPORT).createNode(EShallowEntityType.METHOD, 0, 1)
				.skipTo(DOT).parseUntilOrEof(TOPLEVEL);
		endMethodEntityOnEventBlock(eventBlocksEnd, reportRecognizer);

		inState(TOPLEVEL)
				.sequence(
						EnumSet.of(SELECTION_SCREEN, PARAMETER, SELECT_OPTIONS))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.endNode();

		RecognizerBase<EAbapParserStates> eventBlockRecognizer = inState(
				TOPLEVEL).sequence(eventBlocks)
				.createNode(EShallowEntityType.METHOD, 0).skipTo(DOT)
				.parseUntilOrEof(STATEMENTS);
		endMethodEntityOnEventBlock(eventBlocksEnd, eventBlockRecognizer);

		// "get reference of" and consorts
		inState(TOPLEVEL, STATEMENTS).sequence(GET, ETokenClass.KEYWORD)
				.createNode(EShallowEntityType.STATEMENT, new int[] { 0, 1 })
				.skipTo(DOT).endNode();

		// get ... event handler
		RecognizerBase<EAbapParserStates> getEventBlockRecognizer = inState(
				TOPLEVEL).sequence(GET, IDENTIFIER)
				.createNode(EShallowEntityType.METHOD, 0).skipTo(DOT)
				.parseUntilOrEof(STATEMENTS);
		endMethodEntityOnEventBlock(eventBlocksEnd, getEventBlockRecognizer);

		// at selection screen event handler (named)
		RecognizerBase<EAbapParserStates> selectionScreenBlockRecognizer = inState(
				TOPLEVEL)
				.sequence(AT, SELECTION_SCREEN)
				.skipTo(DOT)
				.createNode(EShallowEntityType.METHOD, new int[] { 0, 1 },
						new Region(0, -2, StringUtils.SPACE))
				.parseUntilOrEof(STATEMENTS);
		endMethodEntityOnEventBlock(eventBlocksEnd,
				selectionScreenBlockRecognizer);

		// at ... event handler (anonymous)
		RecognizerBase<EAbapParserStates> atEventBlockRecognizer = inState(
				TOPLEVEL)
				.sequence(AT, EnumSet.of(LINE_SELECTION, USER_COMMAND))
				.createNode(EShallowEntityType.METHOD, new int[] { 0, 1 })
				.skipTo(DOT).parseUntilOrEof(STATEMENTS);
		endMethodEntityOnEventBlock(eventBlocksEnd, atEventBlockRecognizer);
	}

	/**
	 * Ends the given recognizer if it either hits one of the given event block
	 * end tokens or a get event block.
	 */
	private void endMethodEntityOnEventBlock(
			EnumSet<ETokenType> eventBlocksEnd,
			RecognizerBase<EAbapParserStates> eventBlockRecognizer) {
		eventBlockRecognizer.sequenceBefore(eventBlocksEnd).endNode();
		eventBlockRecognizer.sequenceBefore(GET, IDENTIFIER).endNode();
	}

	/** Rules for parsing of meta elements. */
	private void createMetaRules() {

		inState(DECLARATIONS)
				.sequence(EnumSet.of(PUBLIC, PROTECTED, PRIVATE), SECTION, DOT)
				.createNode(EShallowEntityType.META, SubTypeNames.VISIBILITY, 0)
				.endNode();

		inAnyState().sequence(EnumSet.of(TYPE_POOLS, TABLES, PARAMETERS))
				.createNode(EShallowEntityType.META, 0).skipTo(DOT).endNode();

		inAnyState().sequence(DEFINE)
				.createNode(EShallowEntityType.META, SubTypeNames.MACRO)
				.skipTo(END_OF_DEFINITION, DOT).endNode();

		inState(DECLARATIONS).sequence(EnumSet.of(INTERFACES, ALIASES))
				.createNode(EShallowEntityType.META, 0).skipTo(DOT).endNode();
	}

	/** Rules for parsing types. */
	private void createTypeRules() {

		// classes
		RecognizerBase<EAbapParserStates> classDefinitionAlternative = inState(
				TOPLEVEL, DECLARATIONS).sequence(CLASS, IDENTIFIER, DEFINITION);
		classDefinitionAlternative
				.sequence(EnumSet.of(LOAD, DEFERRED, LOCAL))
				.createNode(EShallowEntityType.TYPE,
						SubTypeNames.CLASS_PUBLICATION, 1).skipTo(DOT)
				.endNode();
		classDefinitionAlternative
				.createNode(EShallowEntityType.TYPE,
						SubTypeNames.CLASS_DEFINITION, 1).skipTo(DOT)
				.parseUntil(DECLARATIONS).sequence(ENDCLASS, DOT).endNode();

		inState(TOPLEVEL, DECLARATIONS)
				.sequence(CLASS, IDENTIFIER, IMPLEMENTATION)
				.createNode(EShallowEntityType.TYPE,
						SubTypeNames.CLASS_IMPLEMENTATION, 1).skipTo(DOT)
				.parseUntil(DECLARATIONS).sequence(ENDCLASS, DOT).endNode();

		// interfaces
		RecognizerBase<EAbapParserStates> interfaceAlternative = inState(
				TOPLEVEL, DECLARATIONS).sequence(INTERFACE, IDENTIFIER);
		interfaceAlternative
				.sequence(EnumSet.of(LOAD, DEFERRED, LOCAL))
				.createNode(EShallowEntityType.TYPE,
						SubTypeNames.INTERFACE_PUBLICATION, 1).skipTo(DOT)
				.endNode();
		interfaceAlternative
				.createNode(EShallowEntityType.TYPE,
						SubTypeNames.INTERFACE_DEFINITION, 1).skipTo(DOT)
				.parseUntil(DECLARATIONS).sequence(ENDINTERFACE, DOT).endNode();

		// types, events, class events
		inState(TOPLEVEL, DECLARATIONS)
				.sequence(EnumSet.of(TYPES, EVENTS, CLASS_EVENTS))
				.createNode(EShallowEntityType.ATTRIBUTE, 0).skipTo(DOT)
				.endNode();
	}

	/** Rules for parsing attributes/methods. */
	private void createMethodAndAttributeRules() {
		inState(TOPLEVEL, DECLARATIONS)
				.sequence(EnumSet.of(CONSTANTS, NODES, STATICS))
				.createNode(EShallowEntityType.ATTRIBUTE, 0, 1).skipTo(DOT)
				.endNode();
		inState(TOPLEVEL, DECLARATIONS)
				.sequence(EnumSet.of(DATA, FIELD_GROUPS, CLASS_DATA))
				.createNode(EShallowEntityType.ATTRIBUTE, 0, 1).skipTo(DOT)
				.endNode();
		inState(TOPLEVEL, DECLARATIONS, STATEMENTS)
				.sequence(EnumSet.of(FIELD_SYMBOLS))
				.createNode(EShallowEntityType.ATTRIBUTE, 0, 1).skipTo(DOT)
				.endNode();

		inState(DECLARATIONS)
				.sequence(EnumSet.of(METHODS, CLASS_METHODS))
				.skipBefore(
						EnumSet.of(DOT, RETURNING, IMPORTING, EXPORTING,
								REDEFINITION, CHANGING, EXCEPTIONS))
				.createNode(EShallowEntityType.METHOD,
						SubTypeNames.METHOD_DECLARATION, new Region(1, -1))
				.skipTo(DOT).endNode();

		inState(DECLARATIONS)
				.sequence(METHOD)
				.markStart()
				.skipBefore(EnumSet.of(DOT, BY))
				.createNode(EShallowEntityType.METHOD,
						SubTypeNames.METHOD_IMPLEMENTATION, new Region(0, -1))
				.skipTo(DOT).parseUntil(STATEMENTS).sequence(ENDMETHOD, DOT)
				.endNode();

		inState(TOPLEVEL, DECLARATIONS)
				.sequence(FUNCTION)
				.markStart()
				.skipTo(DOT)
				.createNode(EShallowEntityType.METHOD, SubTypeNames.FUNCTION,
						new Region(0, -2)).parseUntil(STATEMENTS)
				.sequence(ENDFUNCTION, DOT).endNode();

		RecognizerBase<EAbapParserStates> moduleRecognizer = inState(TOPLEVEL)
				.sequence(MODULE).markStart()
				.skipBefore(EnumSet.of(INPUT, OUTPUT, DOT));
		moduleRecognizer
				.sequence(INPUT, DOT)
				.createNode(EShallowEntityType.METHOD,
						SubTypeNames.MODULE_INPUT, 0).parseUntil(STATEMENTS)
				.sequence(ENDMODULE, DOT).endNode();
		moduleRecognizer
				.sequence(OUTPUT, DOT)
				.createNode(EShallowEntityType.METHOD,
						SubTypeNames.MODULE_OUTPUT, 0).parseUntil(STATEMENTS)
				.sequence(ENDMODULE, DOT).endNode();
		moduleRecognizer
				.subRecognizer(new ImplicitInputModuleRecognizer(), 1, 1)
				.createNode(EShallowEntityType.METHOD,
						SubTypeNames.MODULE_INPUT, 0).parseUntil(STATEMENTS)
				.sequence(ENDMODULE, DOT).endNode();

		inState(TOPLEVEL).sequence(FORM)
				.createNode(EShallowEntityType.METHOD, SubTypeNames.FORM, 1)
				.skipTo(DOT).parseUntil(STATEMENTS).sequence(ENDFORM, DOT)
				.endNode();
	}

	/** Rules for parsing statements. */
	private void createStatementRules() {

		// special rule that matches assignments to variables that have the same
		// name as keywords.
		inState(STATEMENTS).sequence(ETokenClass.KEYWORD, EQ)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.endNode();

		// if/elseif
		RecognizerBase<EAbapParserStates> ifAlternative = inState(TOPLEVEL,
				STATEMENTS).sequence(EnumSet.of(IF, ELSEIF))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS)
				.sequenceBefore(EnumSet.of(ELSEIF, ELSE, ENDIF));
		ifAlternative.sequence(ENDIF, DOT).endNode();
		ifAlternative.endNodeWithContinuation();

		// else
		inState(TOPLEVEL, STATEMENTS).sequence(ELSE)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(EnumSet.of(ENDIF, ENDON), DOT)
				.endNode();

		// case/when
		inState(TOPLEVEL, STATEMENTS).sequence(CASE)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDCASE, DOT).endNode();
		// we parse when as meta, so we add no additional nesting
		inState(STATEMENTS).sequence(WHEN)
				.createNode(EShallowEntityType.META, 0).skipTo(DOT).endNode();

		// on change
		RecognizerBase<EAbapParserStates> changeAlternative = inAnyState()
				.sequence(ON, CHANGE, OF)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.ON_CHANGE).skipTo(DOT)
				.parseUntil(STATEMENTS).sequenceBefore(EnumSet.of(ELSE, ENDON));
		changeAlternative.sequence(ENDON, DOT).endNode();
		changeAlternative.endNodeWithContinuation();

		// loops
		inState(TOPLEVEL, STATEMENTS).sequence(LOOP)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDLOOP, DOT).endNode();
		inState(TOPLEVEL, STATEMENTS).sequence(DO)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDDO, DOT).endNode();
		inState(TOPLEVEL, STATEMENTS).sequence(WHILE)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDWHILE, DOT).endNode();
		inState(STATEMENTS).sequence(AT)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDAT, DOT).endNode();

		// loop likes
		inAnyState().sequence(PROVIDE)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDPROVIDE, DOT).endNode();
		inAnyState().sequence(ENHANCEMENT)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDENHANCEMENT, DOT).endNode();
		inAnyState().sequence(ENHANCEMENT_SECTION)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(END_ENHANCEMENT_SECTION, DOT)
				.endNode();

		// try/catch
		RecognizerBase<EAbapParserStates> tryAlternative = inState(TOPLEVEL,
				STATEMENTS).sequence(EnumSet.of(TRY, CATCH, CLEANUP))
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.parseUntil(STATEMENTS)
				.sequenceBefore(EnumSet.of(ENDTRY, CATCH, ENDCATCH, CLEANUP));
		tryAlternative.sequence(EnumSet.of(ENDTRY, ENDCATCH), DOT).endNode();
		tryAlternative.endNodeWithContinuation();

		createSelectRules();

		// exec
		inState(TOPLEVEL, STATEMENTS)
				.sequence(EXEC, SQL)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.NATIVE_SQL).skipTo(ENDEXEC, DOT).endNode();

		// simple statements that start with a field symbol, e.g.
		// "<fs>-foo = 12."
		inState(TOPLEVEL, STATEMENTS)
				.sequence(LT, SIMPLE_STATEMENT_START_TOKENS)
				.createNode(EShallowEntityType.STATEMENT, new Region(0, 2))
				.skipTo(DOT).endNode();

		inState(TOPLEVEL, STATEMENTS).sequence(SIMPLE_STATEMENT_START_TOKENS)
				.createNode(EShallowEntityType.STATEMENT, 0).skipTo(DOT)
				.endNode();
	}

	/**
	 * Creates the parsing rules for the select clause. This is tricky, because
	 * the rules whether a select block or a single statement select is
	 * expected, are not trivial.
	 */
	private void createSelectRules() {
		RecognizerBase<EAbapParserStates> selectAlternative = inState(TOPLEVEL,
				STATEMENTS).sequence(SELECT);
		selectAlternative.sequence(LPAREN)
				.createNode(EShallowEntityType.STATEMENT, "method call")
				.skipToWithNesting(RPAREN, LPAREN, RPAREN).skipTo(DOT)
				.endNode();
		selectAlternative
				.subRecognizer(new SingleSelectRecognizer(), 1, 1)
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.SINGLE_SELECT).endNode();
		selectAlternative
				.createNode(EShallowEntityType.STATEMENT,
						SubTypeNames.SELECT_BLOCK).skipTo(DOT)
				.parseUntil(STATEMENTS).sequence(ENDSELECT, DOT).endNode();
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFilteredToken(IToken token, IToken previousToken) {
		return super.isFilteredToken(token, previousToken)
				|| token.getType() == PRAGMA_DIRECTIVE;
	}

	/**
	 * Recognizer for MODULE declarations that have neither INPUT nor OUTPUT
	 * keyword between the module name and the DOT. The only thing that
	 * differentiates these declarations from module invocations is that they
	 * are followed by ENDMODULE. later in the code.
	 */
	private static class ImplicitInputModuleRecognizer extends
			RecognizerBase<EAbapParserStates> {

		/** {@inheritDoc} */
		@Override
		protected int matchesLocally(
				ParserState<EAbapParserStates> parserState,
				List<IToken> tokens, int startOffset) {

			if (startOffset < tokens.size()
					&& tokens.get(startOffset).getType() != DOT) {
				return NO_MATCH;
			}

			for (int i = startOffset + 1; i < tokens.size(); i++) {
				switch (tokens.get(i).getType()) {
				case MODULE:
					return NO_MATCH;
				case ENDMODULE:
					return startOffset + 1;
				}
			}
			return NO_MATCH;
		}
	}

	/**
	 * Recognizer that matches single statements selects according to the rules
	 * found <a
	 * href="http://help.sap.com/abapdocu_702/en/abapselect.htm">here</a>. The
	 * recognizer should be called directly after finding the SELECT keyword.
	 */
	private static class SingleSelectRecognizer extends
			RecognizerBase<EAbapParserStates> {

		/**
		 * Token types to be skipped from the select start to reach the result
		 * description.
		 */
		private static final EnumSet<ETokenType> SELECT_TO_RESULTS_SKIP_TOKENS = EnumSet
				.of(SINGLE, FOR, UPDATE, DISTINCT);

		/** Token types for aggregate functions. */
		private static final EnumSet<ETokenType> AGGREGATE_FUNCTIONS = EnumSet
				.of(MIN, MAX, SUM, AVG, COUNT);

		/** Token types that terminate the aggregate functions. */
		private static final EnumSet<ETokenType> AGGREGATE_TERMINATOR = EnumSet
				.of(FROM, INTO);

		/** {@inheritDoc} */
		@Override
		protected int matchesLocally(
				ParserState<EAbapParserStates> parserState,
				List<IToken> tokens, int startOffset) {
			int dotOffset = startOffset;
			while (dotOffset < tokens.size()
					&& tokens.get(dotOffset).getType() != DOT) {
				dotOffset += 1;
			}

			// no match if closing dot was not found
			if (dotOffset >= tokens.size()) {
				return NO_MATCH;
			}

			int matchSingleSelect = dotOffset + 1;

			// the following is statements correspond directly to the rules in
			// http://help.sap.com/abapdocu_702/en/abapselect.htm, where a
			// result of matchSingleSelect means that no ENDSELECT is expected,
			// while a NO_MATCH indicates that an ENDSELECT is required
			if (!hasIntoAppendingTable(tokens, startOffset, dotOffset)) {
				if (isSingle(tokens, startOffset)
						|| (hasOnlyAggregateFunctions(tokens, startOffset,
								dotOffset) && !hasGroupBy(tokens, startOffset,
								dotOffset))) {
					return matchSingleSelect;
				}
				return NO_MATCH;
			}
			if (hasPackageSize(tokens, startOffset, dotOffset)) {
				return NO_MATCH;
			}
			return matchSingleSelect;
		}

		/** Returns whether the SINGLE keyword was found right at the start. */
		private boolean isSingle(List<IToken> tokens, int startOffset) {
			return tokens.get(startOffset).getType() == SINGLE;
		}

		/** Returns whether this has the INTO|APPEND ... TABLE clause. */
		private boolean hasIntoAppendingTable(List<IToken> tokens,
				int startOffset, int endOffset) {
			return TokenStreamUtils.containsAny(tokens, startOffset, endOffset,
					INTO, APPENDING)
					&& TokenStreamUtils.containsAny(tokens, startOffset,
							endOffset, TABLE);
		}

		/** Returns whether this has the PACKAGE SIZE clause. */
		private boolean hasPackageSize(List<IToken> tokens, int startOffset,
				int endOffset) {
			return TokenStreamUtils.containsSequence(tokens, startOffset,
					endOffset, PACKAGE, SIZE);
		}

		/** Returns whether this has the GROUP BY clause. */
		private boolean hasGroupBy(List<IToken> tokens, int startOffset,
				int endOffset) {
			return TokenStreamUtils.containsSequence(tokens, startOffset,
					endOffset, GROUP, BY);
		}

		/** Returns whether this only contains aggregate functions. */
		private boolean hasOnlyAggregateFunctions(List<IToken> tokens,
				int startOffset, int endOffset) {

			while (startOffset < endOffset
					&& SELECT_TO_RESULTS_SKIP_TOKENS.contains(tokens.get(
							startOffset).getType())) {
				startOffset += 1;
			}

			while (startOffset < endOffset
					&& !AGGREGATE_TERMINATOR.contains(tokens.get(startOffset)
							.getType())) {
				if (!AGGREGATE_FUNCTIONS.contains(tokens.get(startOffset)
						.getType())) {
					// found non-aggregate
					return false;
				}
				startOffset = skipAggregate(tokens, startOffset + 1, endOffset);
			}
			return true;
		}

		/**
		 * Skips the remainder of an aggregate function, i.e. a block in
		 * parentheses and the optional AS part. Returns the new startOffset.
		 */
		private int skipAggregate(List<IToken> tokens, int startOffset,
				int endOffset) {
			if (startOffset >= endOffset
					|| tokens.get(startOffset).getType() != LPAREN) {
				return startOffset;
			}
			int rparenPos = TokenStreamUtils.find(tokens, startOffset, endOffset,
					RPAREN);
			if (rparenPos == TokenStreamUtils.NOT_FOUND) {
				return startOffset;
			}

			startOffset = rparenPos + 1;

			// optionally skip AS part
			if (startOffset < endOffset
					&& tokens.get(startOffset).getType() == AS) {
				startOffset += 2;
			}

			return startOffset;
		}
	}
}
