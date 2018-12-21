/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CLikeDefUseHeuristicBase.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.clike;

import static org.conqat.lib.scanner.ETokenType.AND;
import static org.conqat.lib.scanner.ETokenType.ANDEQ;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.DIVEQ;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.EQ;
import static org.conqat.lib.scanner.ETokenType.GT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LSHIFTEQ;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.MINUSEQ;
import static org.conqat.lib.scanner.ETokenType.MINUSMINUS;
import static org.conqat.lib.scanner.ETokenType.MODEQ;
import static org.conqat.lib.scanner.ETokenType.MULTEQ;
import static org.conqat.lib.scanner.ETokenType.OREQ;
import static org.conqat.lib.scanner.ETokenType.PLUSEQ;
import static org.conqat.lib.scanner.ETokenType.PLUSPLUS;
import static org.conqat.lib.scanner.ETokenType.PREPROCESSOR_DIRECTIVE;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.RSHIFTEQ;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.URSHIFTEQ;
import static org.conqat.lib.scanner.ETokenType.XOREQ;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.HeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStreamParser;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStreamSplitter;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for def-use heuristics of C-like languages. Handles the variable
 * scope and several common parsing tasks.
 * 
 * The definitions are extracted from variable definitions (
 * <code>Object a = null</code>) and assignments (<code>b = 12</code>) and
 * stored in a running set that contains all identifiers defined so far. The
 * uses are inferred by scanning the token stream for identifiers that are in
 * the known identifier set.
 * 
 * Scope changes are also handled by this class, i.e. an identifier defined in
 * an <code>if</code> statement is removed from the running set after the
 * <code>if</code> ends.
 * 
 * Example code that will NOT be parsed correctly by this class:
 * <ul>
 * <li><code>int b; b a = (b) c;</code> where <code>b</code> is the name of a
 * type AND a variable.
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51545 $
 * @ConQAT.Rating YELLOW Hash: 8866C887CB0AAE4FA61383A18712AEA1
 */
public abstract class CLikeDefUseHeuristicBase extends HeuristicBase implements
		IDefUseHeuristic {

	/** The assignment operator. */
	private static final ETokenType ASSIGNMENT_OPERATOR = EQ;

	/** Contains all modifying two-sided assignment operators. */
	private static final EnumSet<ETokenType> MODIFICATION_OPERATOR_SET = EnumSet
			.of(PLUSEQ, MINUSEQ, MULTEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ,
					LSHIFTEQ, RSHIFTEQ, URSHIFTEQ);

	/** Contains all two-sided assignment operators. */
	private static final EnumSet<ETokenType> TWO_SIDED_ASSIGNMENT_OPERATOR_SET;
	static {
		TWO_SIDED_ASSIGNMENT_OPERATOR_SET = EnumSet.of(ASSIGNMENT_OPERATOR);
		TWO_SIDED_ASSIGNMENT_OPERATOR_SET.addAll(MODIFICATION_OPERATOR_SET);
	}

	/**
	 * All tokens that represent Java operators that modify a value but do not
	 * have a right side.
	 */
	private static final EnumSet<ETokenType> SELF_MODIFICATION_OPERATORS = EnumSet
			.of(PLUSPLUS, MINUSMINUS);

	/**
	 * Matches all self-modifications of a variable (a++, --a, etc.). Group 0
	 * contains the matched variable.
	 */
	public static final TokenPattern SELF_MODIFICATION_PATTERN = new TokenPattern()
			.alternative(
					new TokenPattern().sequence(SELF_MODIFICATION_OPERATORS)
							.sequence(IDENTIFIER).group(0)
							.notFollowedBy(EnumSet.of(DOT, LPAREN, LT)),
					new TokenPattern().notPrecededBy(EnumSet.of(DOT))
							.sequence(IDENTIFIER).group(0)
							.sequence(SELF_MODIFICATION_OPERATORS));

	/**
	 * The scope stack of known identifiers. Each set represents a scope. The
	 * topmost scope was opened last.
	 */
	private final Stack<Set<String>> scopes = new Stack<Set<String>>();

	/** The possible modifiers before a variable type. */
	private final EnumSet<ETokenType> typeModifiers;

	/** The primitive types supported by the language (i.e. keyword types). */
	private final EnumSet<ETokenType> primitiveTypes;

	/**
	 * The tokens that may appear inside a generic's angular brackets (excluding
	 * the brackets themselves).
	 */
	private final EnumSet<ETokenType> genericContent;

	/** All member dereference operators. */
	private final EnumSet<ETokenType> dereferenceOperators;

	/**
	 * Tokens that are not allowed before an identifier.
	 */
	private final EnumSet<ETokenType> notAllowedBeforeIdentifier;

	/**
	 * Opens the method scope.
	 * 
	 * @param typeModifiers
	 *            the possible modifiers before and after a variable type.
	 * @param primitiveTypes
	 *            the primitive types supported by the language (i.e. keyword
	 *            types).
	 * @param genericContent
	 *            the tokens that may appear inside a generic's angular brackets
	 *            (excluding the brackets themselves).
	 * @param dereferenceOperators
	 *            all member dereference operators.
	 * @param notAllowedBeforeIdentifier
	 *            tokens that are not allowed before an identifier.
	 */
	public CLikeDefUseHeuristicBase(EnumSet<ETokenType> typeModifiers,
			EnumSet<ETokenType> primitiveTypes,
			EnumSet<ETokenType> genericContent,
			EnumSet<ETokenType> dereferenceOperators,
			EnumSet<ETokenType> notAllowedBeforeIdentifier, IConQATLogger logger) {
		super(logger);
		this.typeModifiers = typeModifiers;
		this.primitiveTypes = primitiveTypes;
		this.genericContent = genericContent;
		this.dereferenceOperators = dereferenceOperators;
		this.notAllowedBeforeIdentifier = notAllowedBeforeIdentifier;
		openNewScope();
	}

	/**
	 * Parses the given parameter list to extract all parameter definitions and
	 * adds them to the method scope.
	 */
	protected VariableReadWriteInfo parseParameterList(
			List<IToken> parameterListTokens, String methodName,
			boolean mustContainTypes) {
		VariableReadWriteInfo info = new VariableReadWriteInfo();
		TokenStreamParser parser = new TokenStreamParser(parameterListTokens,
				EnumSet.of(PREPROCESSOR_DIRECTIVE));

		while (!parser.isDone()) {
			if (!skipAnnotations(parser)) {
				logger.error(DataflowExceptionUtils.createMessage(
						"Found broken annotation in parameter list",
						parameterListTokens));
				skipToNextComma(parser);
				continue;
			}

			String type = skipType(parser);
			String keyword = parser.consumeOneOrZeroOf(EnumSet
					.copyOf(ETokenType.KEYWORDS));
			String identifier = parser.consumeOneOrZeroOf(EnumSet
					.of(IDENTIFIER));
			if (keyword != null) {
				// some languages allow variables to be named like keywords,
				// e.g. C#. Ignore those and skip to the next comma
			} else if (identifier == null) {
				if (mustContainTypes) {
					// some unexpected tokens occurred, so we just skip to the
					// next
					// comma and proceed
					logger.error(DataflowExceptionUtils.createMessage(
							"Expected an identifier after a type in the parameter list of method '"
									+ methodName + "', not "
									+ parser.currentType(), parameterListTokens));
				} else {
					addToScope(type);
					info.getDefinitions().add(
							new VariableWrite(type).setOther());
				}
			} else {
				addToScope(identifier);
				info.getDefinitions().add(
						new VariableWrite(identifier).setOther());
			}

			skipToNextComma(parser);
		}

		return info;
	}

	/**
	 * Skips the parser forward after the next comma it finds.
	 */
	private void skipToNextComma(TokenStreamParser parser) {
		parser.consumeAnyExcept(EnumSet.of(COMMA));
		parser.consumeOneOrZeroOf(EnumSet.of(COMMA));
	}

	/**
	 * Parses the given statement tokens and returns the
	 * {@link VariableReadWriteInfo} for it.
	 */
	@Override
	public VariableReadWriteInfo parseStatement(List<IToken> tokens) {
		IToken lastToken = CollectionUtils.getLast(tokens);
		if (lastToken != null && lastToken.getType() == SEMICOLON) {
			tokens = tokens.subList(0, tokens.size() - 1);
		}

		VariableReadWriteInfo info = new VariableReadWriteInfo();
		if (tokens.isEmpty()) {
			return info;
		}

		List<String> readVariables = parseReads(tokens);
		List<String> writtenVariables = parseAssignmentsAndDefinitions(tokens,
				info);
		for (String writtenVariable : writtenVariables) {
			readVariables.remove(writtenVariable);
		}
		info.getReads().addAll(readVariables);
		info.getDereferences().addAll(parseDereferences(tokens));
		return augmentWithSelfModifications(info, tokens);
	}

	/**
	 * Parses all definitions and assignments in the given token stream and adds
	 * them to the {@link VariableReadWriteInfo}.
	 * 
	 * Assignments and definitions are both parsed the same way: First, split
	 * the token stream into different levels according to nested parentheses
	 * "(" and ")", as well as brackets "[" and "]". Then parse each resulting
	 * stream separately by splitting it at "," to separate possible definitions
	 * and then parse any assignment chains like <code>a = b = 2</code> that
	 * might be in those comma separated parts.
	 * 
	 * @return the variables that were written but not read, and should
	 *         therefore not be counted as reads in the final
	 *         {@link VariableReadWriteInfo}.
	 */
	protected List<String> parseAssignmentsAndDefinitions(List<IToken> tokens,
			VariableReadWriteInfo info) {
		int sizeOfLeadingType = getSizeOfLeadingType(tokens);
		boolean isDefinitionStatement = (sizeOfLeadingType > 0);
		if (isDefinitionStatement) {
			// remove leading type
			tokens = tokens.subList(sizeOfLeadingType, tokens.size());
		}

		List<String> writtenAndNotReadVariables = new ArrayList<String>();
		TokenStreamSplitter splitter = new TokenStreamSplitter(tokens);
		splitter.splitNested(LPAREN, RPAREN);
		splitter.splitNested(LBRACK, RBRACK);
		List<List<IToken>> tokenStreams = splitter.getTokenStreams();

		for (int i = 0; i < tokenStreams.size(); i++) {
			List<IToken> splitStream = tokenStreams.get(i);
			boolean isDefinitionPart = (i == 0 && isDefinitionStatement);
			List<List<IToken>> commaSeparatedParts = TokenStreamUtils.split(
					splitStream, COMMA);

			for (List<IToken> commaSeparatedPart : commaSeparatedParts) {
				if (!TokenStreamUtils.containsAny(tokens,
						TWO_SIDED_ASSIGNMENT_OPERATOR_SET)) {
					if (isDefinitionPart) {
						parseEmptyDefinition(commaSeparatedPart, info,
								writtenAndNotReadVariables);
					}
				} else {
					parseAssignmentChain(commaSeparatedPart, isDefinitionPart,
							info, writtenAndNotReadVariables);
				}
			}
		}

		return writtenAndNotReadVariables;
	}

	/**
	 * Parses the given tokens as an assignment chain (e.g.
	 * <code>a = b = c.x().r = y + 22</code>) and fills the info object and the
	 * list of written and not read variables.
	 * 
	 * In an assignment chain, e.g. <code>a = b = c = d + e</code>, only the
	 * variables in the very last part, here d and e, are counted as read, since
	 * the value of e.g. b is not actually used - the assigned value is just
	 * passed on from c.
	 * 
	 * The exception to this are all self-modifying assignment operators, e.g.
	 * <code>a += b += c</code>. Here a and b are indeed read in order to
	 * compute <code>a + b</code> and <code>b + c</code>.
	 * 
	 * @param assignmentChain
	 *            the tokens to parse.
	 * @param isDefinitionPart
	 *            whether the given chain is part of a variable definition, i.e.
	 *            the first assigned variable need not be known yet.
	 */
	private void parseAssignmentChain(List<IToken> assignmentChain,
			boolean isDefinitionPart, VariableReadWriteInfo info,
			List<String> writtenAndNotReadVariables) {
		List<Integer> operatorPositions = TokenStreamUtils.findAll(
				assignmentChain, TWO_SIDED_ASSIGNMENT_OPERATOR_SET);
		List<List<IToken>> assignmentParts = TokenStreamUtils.split(assignmentChain,
				TWO_SIDED_ASSIGNMENT_OPERATOR_SET);

		for (int k = 0; k < assignmentParts.size() - 1; k++) {
			boolean isFirstAssignmentOfDefinition = (isDefinitionPart && k == 0);

			List<IToken> leftSide = assignmentParts.get(k);
			String leftIdentifier = extractSingleTokenText(leftSide,
					ETokenClass.IDENTIFIER);
			if (leftIdentifier == null) {
				continue;
			}
			if (!isFirstAssignmentOfDefinition && !isKnown(leftIdentifier)) {
				continue;
			}

			VariableWrite write = new VariableWrite(leftIdentifier);
			List<IToken> rightSide = assignmentParts.get(k + 1);
			if (!rightSide.isEmpty() && rightSide.get(0).getType() == AND) {
				// remove pointer dereference operator if present
				rightSide = rightSide.subList(1, rightSide.size());
			}

			String rightIdentifier = extractSingleTokenText(rightSide,
					ETokenClass.IDENTIFIER);
			String rightLiteral = extractSingleTokenText(rightSide,
					ETokenClass.LITERAL);
			String newOperator = extractFirstToken(rightSide, ETokenType.NEW);

			IToken operator = assignmentChain.get(operatorPositions.get(k));
			if (operator.getType() == ASSIGNMENT_OPERATOR) {
				if (rightIdentifier != null && isKnown(rightIdentifier)) {
					write.setVariable(rightIdentifier);
				}

				if (rightLiteral != null) {
					if (rightLiteral.equals("null")) {
						write.setNull();
					} else {
						write.setValue(rightLiteral);
					}
				}

				if (newOperator != null) {
					write.setValue(VariableWrite.NEW_OBJECT_VALUE);
				}

				writtenAndNotReadVariables.add(leftIdentifier);
			}

			if (isFirstAssignmentOfDefinition) {
				info.getDefinitions().add(write);
				addToScope(write.getChangedVariable());
			} else {
				info.getAssignments().add(0, write);
			}
		}
	}

	/**
	 * Returns the text of the first token in the given stream, if it has the
	 * given type. Otherwise, returns <code>null</code>.
	 */
	private String extractFirstToken(List<IToken> tokens, ETokenType type) {
		if (tokens.isEmpty()) {
			return null;
		}
		IToken token = tokens.get(0);
		if (token.getType() == type) {
			return token.getText();
		}
		return null;
	}

	/**
	 * Parses the given tokens as an empty variable definition, e.g.
	 * <code>int a;</code> and fills the given info and list.
	 * 
	 * @param commaSeparatedPart
	 *            the tokens of the definition with the leading type removed
	 *            (i.e. only the variable token).
	 */
	private void parseEmptyDefinition(List<IToken> commaSeparatedPart,
			VariableReadWriteInfo info, List<String> writtenAndNotReadVariables) {
		String definedVariable = extractSingleTokenText(commaSeparatedPart,
				ETokenClass.IDENTIFIER);
		if (definedVariable != null) {
			info.getDefinitions().add(
					new VariableWrite(definedVariable).setEmpty());
			writtenAndNotReadVariables.add(definedVariable);
			addToScope(definedVariable);
		}
	}

	/**
	 * If the given token stream consists of a single token of the given token
	 * class, returns that token's text. Otherwise returns <code>null</code>.
	 */
	private String extractSingleTokenText(List<IToken> tokens, ETokenClass klass) {
		if (tokens.size() != 1) {
			return null;
		}

		IToken token = tokens.get(0);
		if (token.getType().getTokenClass() != klass) {
			return null;
		}
		return token.getText();
	}

	/**
	 * If the given tokens represent an variable definition, returns the number
	 * of tokens taken up by the variable's type (and an optional annotation
	 * attached to it). If not, returns 0.
	 */
	private int getSizeOfLeadingType(List<IToken> tokens) {
		TokenStreamParser parser = new TokenStreamParser(tokens);
		if (!skipAnnotations(parser)) {
			return 0;
		}
		if (skipType(parser) == null) {
			return 0;
		}
		if (!parser.isAnyOf(EnumSet.of(IDENTIFIER))) {
			return 0;
		}
		return parser.getConsumedTokenCount();
	}

	/**
	 * Adds writes for all variable self modifications, e.g. <code>a++</code>.
	 */
	private VariableReadWriteInfo augmentWithSelfModifications(
			VariableReadWriteInfo info, List<IToken> tokens) {
		List<TokenPatternMatch> matches = SELF_MODIFICATION_PATTERN
				.match(tokens);
		List<String> identifiers = TokenPatternMatch.getAllStrings(matches, 0);
		for (String identifier : identifiers) {
			if (isKnown(identifier)) {
				info.getAssignments().add(new VariableWrite(identifier));
			}
		}
		return info;
	}

	/**
	 * Scans the given token stream for dereferences of known variables.
	 * 
	 * A variable is dereferenced if
	 * <ul>
	 * <li>the "dot" operator is applied to it
	 * <li>it is an array and one of its members is accessed using "[]"
	 * <li>it is incremented/decremented with "++" or "--"
	 * <li>it is assigned a value based on a modification of itself, e.g. with
	 * "+=" or "*="
	 * </ul>
	 */
	protected List<String> parseDereferences(List<IToken> tokens) {
		List<String> dereferencedIdentifiers = new ArrayList<String>();
		List<TokenPatternMatch> matches = new TokenPattern().alternative(
				new TokenPattern()
						.notPrecededBy(notAllowedBeforeIdentifier)
						.sequence(IDENTIFIER)
						.group(0)
						.alternative(dereferenceOperators,
								SELF_MODIFICATION_OPERATORS,
								MODIFICATION_OPERATOR_SET),
				new TokenPattern().sequence(SELF_MODIFICATION_OPERATORS)
						.sequence(IDENTIFIER).group(0)
						.notFollowedBy(dereferenceOperators)
						.notFollowedBy(LPAREN)).match(tokens);
		List<String> identifiers = TokenPatternMatch.getAllStrings(matches, 0);
		for (String identifier : identifiers) {
			if (isKnown(identifier)) {
				dereferencedIdentifiers.add(identifier);
			}
		}
		return dereferencedIdentifiers;
	}

	/**
	 * Parses annotations in the language, if at least one is present. Skips
	 * over all subsequent annotations until the first token is reached that
	 * does not belong to an annotation.
	 * 
	 * @return <code>true</code> if there was no annotation and the parser was
	 *         not modified, <code>true</code> if there was at least one
	 *         annotation and the parser was moved past all of them,
	 *         <code>false</code> if there was something that looked like an
	 *         annotation, but wasn't and the parser is at an undefined
	 *         position.
	 */
	protected abstract boolean skipAnnotations(TokenStreamParser parser);

	/** Opens a new scope. */
	@Override
	public void openNewScope() {
		scopes.push(new HashSet<String>());
	}

	/** Closes the currently open scope. */
	@Override
	public void closeCurrentScope() {
		scopes.pop();
		CCSMAssert.isTrue(scopes.size() > 0, "Cannot close the method scope");
	}

	/**
	 * Adds the given identifier to the current scope. Should be called for each
	 * variable or parameter definition.
	 */
	@Override
	public void addToScope(String identifier) {
		scopes.peek().add(identifier);
	}

	/** {@inheritDoc} */
	@Override
	public boolean isKnown(String identifier) {
		for (Set<String> scope : scopes) {
			if (scope.contains(identifier)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Skips all tokens of the type starting at the current parser location,
	 * including any leading modifiers, e.g. "final", and any following generic
	 * type arguments, if present.
	 * 
	 * Furthermore, any array notation and the optional ellipsis are skipped,
	 * e.g. <code>String[][]...</code>
	 * 
	 * @return A part of the skipped type if there actually was a type that was
	 *         successfully skipped or <code>null</code> if there was anything
	 *         else, parts of which may have been skipped. The returned string
	 *         may be used in case there wasn't actually a type at that location
	 *         but a variable.
	 */
	protected String skipType(TokenStreamParser parser) {
		String baseType = null;

		parser.consumeAnyOf(typeModifiers);
		String primitiveType = parser.consumeOneOrZeroOf(primitiveTypes);

		if (primitiveType == null) {
			// C# allows generic arguments in the middle of a type expression,
			// e.g. "A<Foo>.B" so we need to skip in a loop here
			do {
				while (true) {
					List<String> baseTypeParts = parser.consumeAlternating(
							EnumSet.of(IDENTIFIER), EnumSet.of(DOT));
					if (baseTypeParts.isEmpty()
							|| CollectionUtils.getLast(baseTypeParts).equals(
									".")) {
						return null;
					}

					if (parser.isAnyOf(EnumSet.of(COLON))) {
						parser.consumeAnyOf(EnumSet.of(COLON));
					} else {
						baseType = StringUtils.concat(baseTypeParts);
						break;
					}
				}

				if (parser.isAnyOf(EnumSet.of(LT))) {
					if (!skipGeneric(parser)) {
						return null;
					}
				}
			} while (parser.consumeOneOrZeroOf(EnumSet.of(DOT)) != null);
		} else {
			baseType = primitiveType;
		}

		parser.consumeAnyOf(typeModifiers);

		// skip array notation
		while (parser.isAnyOf(EnumSet.of(LBRACK))) {
			parser.consumeOneOrZeroOf(EnumSet.of(LBRACK));
			// C# allows specifying multi-dimensional arrays like foo[,,][,,]
			parser.consumeAnyOf(EnumSet.of(COMMA));
			if (parser.consumeOneOrZeroOf(EnumSet.of(RBRACK)) == null) {
				// is an array access, not array notation
				return null;
			}
		}

		parser.consumeAnyOf(typeModifiers);
		return baseType;
	}

	/**
	 * Tries to skip the type parameters of a generic, i.e.
	 * <code><Foo, Bar<Goo & Test>></code> etc. This method just assumes that
	 * the current token is an {@link ETokenType#LT}, so callers should check
	 * for that.
	 * 
	 * @return <code>true</code> if it was indeed a generic, <code>false</code>
	 *         if skipping stopped prematurely.
	 */
	private boolean skipGeneric(TokenStreamParser parser) {
		return parser.skipBalanced(LT, GT, genericContent);
	}

	/**
	 * Scans the given token stream for reads of known variables.
	 */
	protected List<String> parseReads(List<IToken> tokens) {
		List<String> readIdentifiers = new ArrayList<String>();
		List<TokenPatternMatch> matches = new TokenPattern()
				.notPrecededBy(notAllowedBeforeIdentifier).sequence(IDENTIFIER)
				.group(0).notFollowedBy(LPAREN).match(tokens);
		List<String> identifiers = TokenPatternMatch.getAllStrings(matches, 0);
		for (String identifier : identifiers) {
			if (isKnown(identifier)) {
				readIdentifiers.add(identifier);
			}
		}
		return readIdentifiers;
	}

}