/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapDefUseHeuristic.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import static org.conqat.lib.scanner.ETokenType.ARROW;
import static org.conqat.lib.scanner.ETokenType.ARROWSTAR;
import static org.conqat.lib.scanner.ETokenType.ASSIGN;
import static org.conqat.lib.scanner.ETokenType.ASSIGNED;
import static org.conqat.lib.scanner.ETokenType.ASSIGNING;
import static org.conqat.lib.scanner.ETokenType.BEGIN;
import static org.conqat.lib.scanner.ETokenType.CAST;
import static org.conqat.lib.scanner.ETokenType.COMPUTE;
import static org.conqat.lib.scanner.ETokenType.CONDENSE;
import static org.conqat.lib.scanner.ETokenType.DATA;
import static org.conqat.lib.scanner.ETokenType.DEFAULT;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.END;
import static org.conqat.lib.scanner.ETokenType.EQ;
import static org.conqat.lib.scanner.ETokenType.EXACT;
import static org.conqat.lib.scanner.ETokenType.FIELD_SYMBOL;
import static org.conqat.lib.scanner.ETokenType.FIELD_SYMBOLS;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IS;
import static org.conqat.lib.scanner.ETokenType.LIKE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.NOT;
import static org.conqat.lib.scanner.ETokenType.OF;
import static org.conqat.lib.scanner.ETokenType.OVERLAY;
import static org.conqat.lib.scanner.ETokenType.REF;
import static org.conqat.lib.scanner.ETokenType.REPLACE;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SHIFT;
import static org.conqat.lib.scanner.ETokenType.STATICS;
import static org.conqat.lib.scanner.ETokenType.TABLE;
import static org.conqat.lib.scanner.ETokenType.TO;
import static org.conqat.lib.scanner.ETokenType.TRANSLATE;
import static org.conqat.lib.scanner.ETokenType.TYPE;
import static org.conqat.lib.scanner.ETokenType.UNASSIGN;
import static org.conqat.lib.scanner.ETokenType.VALUE;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;

import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Uses a heuristic to find identifier definitions and uses.
 * 
 * The definitions are extracted from variable definitions (data, constants,
 * statics, field-symbols) and assignments (<code>b = 12. move 12 to b.</code>
 * etc.) and stored in a running set that contains all identifiers defined so
 * far. The uses are inferred by scanning the token stream for identifiers that
 * are in the known identifier set.
 * 
 * Since the scope of a variable are all statements occurring after the
 * definition statement, regardless of the nesting, there is no need to keep
 * track of variable scopes. E.g. a variable defined inside an if may be used
 * after the if.
 * 
 * Example code that will NOT be parsed correctly by this class:
 * <ul>
 * <li>changing a variable using a field symbol will not be registered as a
 * write to the variable, e.g. the <code>clear</code> keyword.
 * <li>variables named like keywords are simply ignored.
 * <li>tables and nodes are treated like regular variables to which no
 * assignments can happen. Modifications of these structures are not counted as
 * assignments, e.g. through the <code>insert</code> statement as e.g. two
 * consecutive inserts would be counted as overwriting the variable, i.e. a
 * useless write.
 * </ul>
 * 
 * The statement patterns were constructed according to
 * http://help.sap.com/abapdocu_740/en/index.htm, Chapter ABAP Keyword
 * Documentation → ABAP Overview → ABAP Statements - Overview
 * 
 * @author $Author: streitel $
 * @version $Rev: 51545 $
 * @ConQAT.Rating YELLOW Hash: E00C4F97356B58C79DDEEA06C68A70C6
 */
public class AbapDefUseHeuristic implements IDefUseHeuristic {

	/**
	 * Matches a dereference of a reference variable. Group 0 contains the
	 * dereferenced variable.
	 */
	private static final TokenPattern REFERENCE_DEREFERENCE_PATTERN = new TokenPattern()
			.notPrecededBy(ARROW).sequence(IDENTIFIER).group(0)
			.sequence(EnumSet.of(ARROW, ARROWSTAR));

	/** Matches all read variables. Group 0 contains the variable name. */
	private static final TokenPattern VARIABLE_READ_PATTERN = new TokenPattern()
			.notPrecededBy(ARROW)
			.sequence(AbapPatterns.FIELD_SYMBOL_OR_DATA_PATTERN).group(0);

	/**
	 * Matches an inline variable definition, e.g. "data(a)". Group 1 contains
	 * the declared variable, group 0 contains all other matched indices.
	 */
	private static final TokenPattern INLINE_DEFINITION_PATTERN = new TokenPattern()
			.sequence(EnumSet.of(DATA, FIELD_SYMBOL), LPAREN).group(0)
			.sequence(AbapPatterns.FIELD_SYMBOL_OR_DATA_PATTERN).group(1)
			.sequence(RPAREN).group(0);

	/**
	 * Matches all dereferenced field symbols, except in statements matched by
	 * {@link #FIELD_SYMBOL_NON_DEREFERENCE_PATTERN}. Group 0 contains the
	 * dereferenced field symbol.
	 */
	private static final TokenPattern FIELD_SYMBOL_DEREFERENCE_PATTERN = new TokenPattern()
			.notPrecededBy(ASSIGNING)
			.sequence(AbapPatterns.FIELD_SYMBOL_PATTERN).group(0);

	/** Matches all statements that do not dereference a field symbol. */
	private static final TokenPattern FIELD_SYMBOL_NON_DEREFERENCE_PATTERN = new TokenPattern()
			.beginningOfStream().alternative(
					ASSIGN,
					UNASSIGN,
					new TokenPattern().sequence(IS).optional(NOT)
							.sequence(ASSIGNED));

	/**
	 * Matches the "default" addition to variable declarations, which assigns a
	 * variable to the declared variable. Group 0 contains the name of the
	 * assigned variable.
	 */
	private static final TokenPattern DEFAULT_VARIABLE_ADDITON_PATTERN = new TokenPattern()
			.sequence(DEFAULT).sequence(ETokenClass.IDENTIFIER).group(0);

	/**
	 * Matches the "value" addition to a variable declaration, which assigns a
	 * literal to the declared variable. Group 0 contains the assigned literal.
	 */
	private static final TokenPattern DEFAULT_VALUE_ADDITION_PATTERN = new TokenPattern()
			.sequence(VALUE).sequence(ETokenClass.LITERAL).group(0);

	/** Matches the "type ref to" addition to variable declarations. */
	private static final TokenPattern TYPE_REF_TO_PATTERN = new TokenPattern()
			.sequence(TYPE, REF, TO);

	/**
	 * Matches a variable declaration with a "like" type qualifier. Group 0
	 * contains the name of the variable that serves as the template for the
	 * declared variable.
	 */
	private static final TokenPattern LIKE_PATTERN = new TokenPattern()
			.sequence(LIKE).alternative(
					new TokenPattern().sequence(IDENTIFIER).group(0),
					new TokenPattern().skipTo(OF, TO).sequence(IDENTIFIER)
							.group(0));

	/**
	 * All keywords that start a variable definition.
	 * 
	 * NOTE: there are more keywords that define "variables", e.g. tables or
	 * nodes. These are, however, not interesting for the data flow analysis.
	 */
	private static final EnumSet<ETokenType> DEFINITION_KEYWORDS = EnumSet.of(
			DATA, STATICS, FIELD_SYMBOLS);

	/** Matches a variable definition. Group 0 contains the variable name. */
	private static final TokenPattern VARIABLE_DEFINITION_PATTERN = new TokenPattern()
			.beginningOfStream().sequence(DEFINITION_KEYWORDS)
			.sequence(AbapPatterns.FIELD_SYMBOL_OR_DATA_PATTERN).group(0);

	/** Matches an "end of" statement. */
	private static final TokenPattern END_OF_PATTERN = new TokenPattern()
			.beginningOfStream().sequence(DEFINITION_KEYWORDS, END, OF);

	/**
	 * Matches a "begin of" statement. Group 0 contains the name of the
	 * structure that was begun.
	 */
	private static final TokenPattern BEGIN_OF_PATTERN = new TokenPattern()
			.beginningOfStream().sequence(DEFINITION_KEYWORDS, BEGIN, OF)
			.optional(ETokenClass.IDENTIFIER).group(0);

	/**
	 * Matches a variable (group 0) or literal (group 1) at the end of the token
	 * stream.
	 */
	private static final TokenPattern VARIABLE_OR_LITERAL_PATTERN = new TokenPattern()
			.beginningOfStream()
			.optional(AbapPatterns.FIELD_SYMBOL_OR_DATA_PATTERN).group(0)
			.optional(ETokenClass.LITERAL).group(1).endOfStream();

	/** Matches the beginning of an assignment. */
	private static final TokenPattern BEGINNING_OF_ASSIGNMENT_PATTERN = new TokenPattern()
			.beginningOfStream()
			.optional(COMPUTE)
			.optional(EXACT)
			.sequence(AbapPatterns.FIELD_SYMBOL_OR_DATA_PATTERN,
					EnumSet.of(EQ, CAST));

	/** The defined variables. */
	private final Set<String> definedVariables = new HashSet<String>();

	/** The defined reference variables. */
	private final Set<String> referenceVariables = new HashSet<String>();

	/**
	 * The current nesting level of <code>data begin of</code> statements. A
	 * level of 0 means that no such statement has been processed yet.
	 */
	private int beginOfLevel = 0;

	/**
	 * Parses the given statement. Chain statements (e.g.
	 * <code>data: ..., ...</code>) must be resolved into separate statements
	 * before calling this function.
	 */
	@Override
	public VariableReadWriteInfo parseStatement(List<IToken> tokens) {
		IToken lastToken = CollectionUtils.getLast(tokens);
		if (lastToken != null && lastToken.getType() == DOT) {
			tokens = tokens.subList(0, tokens.size() - 1);
		}

		VariableReadWriteInfo info = new VariableReadWriteInfo();
		if (tokens.isEmpty()) {
			return info;
		}

		tokens = parseInlineDefinitions(tokens, info);
		parseFieldSymbolDereferences(tokens, info);
		parseReferenceDereferences(tokens, info);
		parseMethodCalls(tokens, info);

		if (parseDefinition(tokens, info)) {
			return info;
		}

		if (parseAssignment(tokens, info)) {
			return info;
		}

		info.getReads().addAll(parseReads(tokens));
		return info;
	}

	/**
	 * Parses all writes due to method calls with importing, changing and
	 * receiving parameters.
	 */
	private void parseMethodCalls(List<IToken> tokens,
			VariableReadWriteInfo info) {
		List<TokenPatternMatch> matches = AbapPatterns.METHOD_PARAMETER_WRITES_PATTERN
				.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variableName = match.groupString(0);
			if (!StringUtils.isEmpty(variableName) && isKnown(variableName)) {
				info.getAssignments().add(new VariableWrite(variableName));
			}
		}
	}

	/**
	 * Parses dereferences of reference variables via the <code>-></code>
	 * operator.
	 */
	private void parseReferenceDereferences(List<IToken> tokens,
			VariableReadWriteInfo info) {
		List<TokenPatternMatch> matches = REFERENCE_DEREFERENCE_PATTERN
				.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variableName = match.groupString(0);
			if (isKnown(variableName)) {
				info.getDereferences().add(variableName);
			}
		}
	}

	/**
	 * Parses all field symbol dereferences. A field symbol is dereferenced
	 * anywhere it occurs, except in assign or unassign statements, assigning
	 * clauses or "is assigned" conditions.
	 */
	private void parseFieldSymbolDereferences(List<IToken> tokens,
			VariableReadWriteInfo info) {
		if (FIELD_SYMBOL_NON_DEREFERENCE_PATTERN.matches(tokens)) {
			return;
		}

		List<TokenPatternMatch> matches = FIELD_SYMBOL_DEREFERENCE_PATTERN
				.match(tokens);
		for (TokenPatternMatch match : matches) {
			String fieldSymbol = match.groupString(0);
			if (isKnown(fieldSymbol)) {
				info.getDereferences().add(fieldSymbol);
			}
		}
	}

	/**
	 * Parses all inline definitions in the given token stream and adds the
	 * defined variables to the {@link #definedVariables} set.
	 * 
	 * @return a purged token stream with inline definitions replaced with the
	 *         defined variable.
	 */
	private List<IToken> parseInlineDefinitions(List<IToken> tokens,
			VariableReadWriteInfo info) {
		List<TokenPatternMatch> matches = INLINE_DEFINITION_PATTERN
				.match(tokens);
		Set<Integer> indicesToRemove = new HashSet<Integer>();
		for (TokenPatternMatch match : matches) {
			indicesToRemove.addAll(match.groupIndices(0));
			String definedVariable = match.groupString(1);
			definedVariables.add(definedVariable);

			info.getDefinitions().add(
					new VariableWrite(definedVariable).setEmpty());
		}

		List<IToken> purgedTokens = new ArrayList<IToken>();
		for (int i = 0; i < tokens.size(); i++) {
			if (!indicesToRemove.contains(i)) {
				purgedTokens.add(tokens.get(i));
			}
		}
		return purgedTokens;
	}

	/**
	 * Parses the given statement as a variable definition and adds the
	 * extracted information to the given {@link VariableReadWriteInfo}.
	 * 
	 * @return <code>true</code> if the statement could be parsed as a
	 *         definition.
	 */
	private boolean parseDefinition(List<IToken> tokens,
			VariableReadWriteInfo info) {
		ETokenType typeOfFirstToken = tokens.get(0).getType();
		if (!DEFINITION_KEYWORDS.contains(typeOfFirstToken)) {
			return false;
		}

		TokenPatternMatch beginOfMatch = BEGIN_OF_PATTERN.matchFirst(tokens);
		if (beginOfMatch != null) {
			String variableName = beginOfMatch.groupString(0);
			if (beginOfLevel == 0 && !StringUtils.isEmpty(variableName)) {
				info.getDefinitions().add(new VariableWrite(variableName));
				definedVariables.add(variableName);
			}

			beginOfLevel += 1;
			return true;
		}

		if (END_OF_PATTERN.matches(tokens)) {
			beginOfLevel -= 1;
			return true;
		}

		if (beginOfLevel > 0) {
			// ignore any nested data definitions
			return true;
		}

		TokenPatternMatch likeMatch = LIKE_PATTERN.matchFirst(tokens);
		if (likeMatch != null) {
			String variable = likeMatch.groupString(0);
			if (isKnown(variable)) {
				info.getReads().add(variable);
			}
		}

		TokenPatternMatch variableNameMatch = VARIABLE_DEFINITION_PATTERN
				.matchFirst(tokens);
		if (variableNameMatch == null) {
			// some weird variable declaration, e.g. the variable name is a
			// keyword, so we just ignore it.
			return true;
		}

		String variableName = variableNameMatch.groupString(0);
		definedVariables.add(variableName);
		boolean isReferenceDeclaration = TYPE_REF_TO_PATTERN.matches(tokens);
		if (isReferenceDeclaration) {
			referenceVariables.add(variableName);
		}

		VariableWrite write = new VariableWrite(variableName).setEmpty();
		write.makeDefaultInitialization();
		if (typeOfFirstToken == FIELD_SYMBOLS || isReferenceDeclaration) {
			// the default value of field-symbols and references is null, not
			// empty.
			write.setNull();
			write.makeDefaultInitialization();
		}

		TokenPatternMatch valueMatch = DEFAULT_VALUE_ADDITION_PATTERN
				.matchFirst(tokens);
		TokenPatternMatch variableMatch = DEFAULT_VARIABLE_ADDITON_PATTERN
				.matchFirst(tokens);
		if (typeOfFirstToken == STATICS) {
			// statics don't always have the given default value.
			write.setOther();
		} else if (valueMatch != null) {
			write.setValue(valueMatch.groupString(0));
		} else if (variableMatch != null) {
			write.setVariable(variableMatch.groupString(0));
		}
		info.getDefinitions().add(write);
		return true;
	}

	/**
	 * Parses the given statement as an assignment and adds the extracted
	 * information to the given {@link VariableReadWriteInfo}.
	 * 
	 * @return <code>true</code> if the statement could be parsed as an
	 *         assignment.
	 */
	private boolean parseAssignment(List<IToken> tokens,
			VariableReadWriteInfo info) {
		if (BEGINNING_OF_ASSIGNMENT_PATTERN.matches(tokens)) {
			parseAssignmentChain(tokens, info);
			return true;
		}

		for (int i = 0; i < AbapPatterns.ASSIGNMENT_PATTERNS.size(); i++) {
			TokenPattern pattern = AbapPatterns.ASSIGNMENT_PATTERNS.get(i);
			TokenPatternMatch match = pattern.matchFirst(tokens);
			if (match == null) {
				continue;
			}

			List<String> reads = parseReads(tokens);
			if (match.hasGroup(AbapPatterns.OTHER_ASSIGNMENT)) {
				// assignment of "other"
				List<String> fixedVariables = fixFieldSymbols(match
						.groupTexts(AbapPatterns.OTHER_ASSIGNMENT));
				for (int k = 0; k < fixedVariables.size(); k++) {
					String variable = fixedVariables.get(k);
					removeWrittenVariable(reads, variable, tokens, k);
					if (isKnown(variable)) {
						info.getAssignments().add(new VariableWrite(variable));
					}
				}
			} else if (match.hasGroup(AbapPatterns.ASSIGNMENT_LEFT)) {
				// assignment of literal or variable
				String writtenVariable = match
						.groupString(AbapPatterns.ASSIGNMENT_LEFT);
				removeWrittenVariable(reads, writtenVariable, tokens, 0);

				if (isKnown(writtenVariable)) {
					VariableWrite write = new VariableWrite(writtenVariable);
					String assignedVariable = match
							.groupString(AbapPatterns.ASSIGNMENT_RIGHT);

					if (isKnown(assignedVariable)) {
						write.setVariable(assignedVariable);
					} else if (!StringUtils.isEmpty(assignedVariable)) {
						write.setValue(assignedVariable);
					}
					info.getAssignments().add(write);
				}
			} else if (match.hasGroup(AbapPatterns.NULLED_FIELD_SYMBOL)) {
				// assignment of null to field symbol
				String writtenVariable = match
						.groupString(AbapPatterns.NULLED_FIELD_SYMBOL);
				if (isKnown(writtenVariable)) {
					info.getAssignments().add(
							new VariableWrite(writtenVariable).setNull());
				}
			} else if (match.hasGroup(AbapPatterns.NULLED_REFERENCE)) {
				// clear of a variable which might be a reference
				String writtenVariable = match
						.groupString(AbapPatterns.NULLED_REFERENCE);
				if (isKnown(writtenVariable)) {
					if (referenceVariables.contains(writtenVariable)) {
						info.getAssignments().add(
								new VariableWrite(writtenVariable).setNull());
					} else {
						info.getAssignments().add(
								new VariableWrite(writtenVariable));
					}
				}
			} else if (match.hasGroup(AbapPatterns.NEW_ASSIGNMENT)) {
				// object/data creation
				String writtenVariable = match
						.groupString(AbapPatterns.NEW_ASSIGNMENT);
				if (isKnown(writtenVariable)) {
					info.getAssignments().add(
							new VariableWrite(writtenVariable)
									.setValue(VariableWrite.NEW_OBJECT_VALUE));
				}
			} else if (match.hasGroup(AbapPatterns.EXCEPTION_ASSIGNMENT)) {
				// object/data creation
				String writtenVariable = match
						.groupString(AbapPatterns.EXCEPTION_ASSIGNMENT);
				if (isKnown(writtenVariable)) {
					info.getAssignments().add(
							new VariableWrite(writtenVariable)
									.setValue(VariableWrite.EXCEPTION_VALUE));
				}
			} else {
				CCSMAssert
						.fail(DataflowExceptionUtils
								.createMessage(
										"Pattern "
												+ i
												+ " matched but did not find any assigned identifiers.",
										tokens));
			}

			info.getReads().addAll(reads);
			return true;
		}

		return false;
	}

	/**
	 * Removes one occurrence of the given variable from the given list of read
	 * variables, unless the variable is the first in a translate, condense,
	 * overlay, text replace or shift statement, in which case the variable will
	 * be read and written.
	 */
	private void removeWrittenVariable(List<String> reads, String variable,
			List<IToken> tokens, int variableIndex) {
		if (variableIndex == 0
				&& EnumSet.of(TRANSLATE, CONDENSE, OVERLAY, REPLACE, SHIFT)
						.contains(tokens.get(0).getType())
				&& !TokenStreamUtils.containsAny(tokens, TABLE)) {
			return;
		}
		reads.remove(variable);
	}

	/**
	 * Given a list of token texts that represent variables, merges all field
	 * symbol texts, e.g. "<", "identifier", ">", into a proper variable name,
	 * e.g. "<identifier>".
	 */
	private List<String> fixFieldSymbols(List<String> variables) {
		List<String> fixedVariables = new ArrayList<String>();
		for (ListIterator<String> iterator = variables.listIterator(); iterator
				.hasNext();) {
			String variable = iterator.next();
			if (variable.equals(">")) {
				continue;
			}

			if (variable.equals("<")) {
				iterator.remove();
				variable = "<" + iterator.next() + ">";
			}
			fixedVariables.add(variable);
		}
		return fixedVariables;
	}

	/**
	 * Parses an assignment chain, e.g. <code>a = b = c = d + 2</code> and adds
	 * the extracted information to the given {@link VariableReadWriteInfo}.
	 */
	private void parseAssignmentChain(List<IToken> tokens,
			VariableReadWriteInfo info) {
		List<List<IToken>> parts = TokenStreamUtils.split(tokens, EQ, CAST);

		for (int i = 0; i < parts.size() - 1; i++) {
			List<IToken> leftSide = parts.get(i);
			String leftVariable = "";
			for (IToken token : leftSide) {
				leftVariable += token.getText();
			}

			List<IToken> rightSide = parts.get(i + 1);
			info.getReads().addAll(parseReads(rightSide));
			if (!isKnown(leftVariable)) {
				// we might be assigning to something like "a-b"
				info.getReads().addAll(parseReads(leftSide));
				continue;
			}

			VariableWrite write = new VariableWrite(leftVariable);
			TokenPatternMatch match = VARIABLE_OR_LITERAL_PATTERN
					.matchFirst(rightSide);
			if (match != null) {
				if (match.hasGroup(0)) {
					write.setVariable(match.groupString(0));
				} else if (match.hasGroup(1)) {
					write.setValue(match.groupString(1));
				}
			}
			info.getAssignments().add(write);
		}
	}

	/**
	 * Returns all referenced variables in the given token stream.
	 */
	private List<String> parseReads(List<IToken> tokens) {
		List<String> identifiers = new ArrayList<String>();
		List<TokenPatternMatch> matches = VARIABLE_READ_PATTERN.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variable = match.groupString(0);
			if (!isKnown(variable) && !variable.startsWith("<")
					&& variable.contains("-")) {
				// take only the first part of a dereference
				variable = variable.split("-")[0];
			}
			if (isKnown(variable)) {
				identifiers.add(variable);
			}
		}
		return identifiers;
	}

	/** {@inheritDoc} */
	@Override
	public boolean isKnown(String variable) {
		return definedVariables.contains(variable);
	}

	/** {@inheritDoc} */
	@Override
	public void addToScope(String variableName) {
		definedVariables.add(variableName);
	}

	/** {@inheritDoc} */
	@Override
	public void openNewScope() {
		// do nothing, ABAP has no traditional scoping
	}

	/** {@inheritDoc} */
	@Override
	public void closeCurrentScope() {
		// do nothing, ABAP has no traditional scoping
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Always returns <code>null</code> as ABAP does not have parameter lists.
	 */
	@Override
	public VariableReadWriteInfo parseParameterList(List<IToken> tokens,
			String methodName) {
		return null;
	}

}
