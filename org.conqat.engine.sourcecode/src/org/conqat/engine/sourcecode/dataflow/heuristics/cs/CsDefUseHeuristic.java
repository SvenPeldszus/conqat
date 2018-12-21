/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsDefUseHeuristic.java 51148 2014-11-14 13:51:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import static org.conqat.lib.scanner.ETokenType.AND;
import static org.conqat.lib.scanner.ETokenType.AT;
import static org.conqat.lib.scanner.ETokenType.BOOL;
import static org.conqat.lib.scanner.ETokenType.BYTE;
import static org.conqat.lib.scanner.ETokenType.CHAR;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.DECIMAL;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.DOUBLE;
import static org.conqat.lib.scanner.ETokenType.DYNAMIC;
import static org.conqat.lib.scanner.ETokenType.ENUM;
import static org.conqat.lib.scanner.ETokenType.EXTENDS;
import static org.conqat.lib.scanner.ETokenType.FLOAT;
import static org.conqat.lib.scanner.ETokenType.GT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.INT;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LONG;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.MULT;
import static org.conqat.lib.scanner.ETokenType.NEW;
import static org.conqat.lib.scanner.ETokenType.OBJECT;
import static org.conqat.lib.scanner.ETokenType.OUT;
import static org.conqat.lib.scanner.ETokenType.PARAMS;
import static org.conqat.lib.scanner.ETokenType.QUESTION;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.REF;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SBYTE;
import static org.conqat.lib.scanner.ETokenType.SHORT;
import static org.conqat.lib.scanner.ETokenType.STRING;
import static org.conqat.lib.scanner.ETokenType.SUPER;
import static org.conqat.lib.scanner.ETokenType.THIS;
import static org.conqat.lib.scanner.ETokenType.UINT;
import static org.conqat.lib.scanner.ETokenType.ULONG;
import static org.conqat.lib.scanner.ETokenType.USHORT;
import static org.conqat.lib.scanner.ETokenType.VAR;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeDefUseHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStreamParser;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Uses a heuristic to find identifier definitions and uses.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: 41D7B75675FC291430CEE119F74E4655
 */
public class CsDefUseHeuristic extends CLikeDefUseHeuristicBase {

	/** Matches reads of functors. Group 0 contains the name of the functor. */
	private static final TokenPattern FUNCTOR_READ_PATTERN = new TokenPattern()
			.notPrecededBy(DOT).sequence(IDENTIFIER).group(0).sequence(LPAREN);

	/**
	 * Matches pointer ("*a") and functor dereference ("a()"). Group 0 contains
	 * the name of the dereferenced variable.
	 */
	private static final TokenPattern POINTER_OR_FUNCTOR_DEREFERENCE_PATTERN = new TokenPattern()
			.alternative(
					new TokenPattern()
							.sequence(MULT)
							.sequence(IDENTIFIER)
							.group(0)
							.notFollowedBy(
									CsConditionHeuristic.DEREFERENCE_OPERATORS),
					new TokenPattern()
							.notPrecededBy(
									CsConditionHeuristic.DEREFERENCE_OPERATORS)
							.sequence(IDENTIFIER).group(0).sequence(LPAREN));

	/**
	 * Matches variables passed as <code>out</code> and <code>ref</code>
	 * parameters in method calls.
	 */
	private static final TokenPattern OUT_REF_PARAMETER_PATTERN = new TokenPattern()
			.sequence(EnumSet.of(OUT, REF)).group(1).sequence(IDENTIFIER)
			.group(0).notFollowedBy(CsConditionHeuristic.DEREFERENCE_OPERATORS);

	/**
	 * Matches anonymous class initializations, e.g.
	 * "new A() { x = 12, y = 13 }". Group 0 contains all tokens in the init
	 * block, i.e. between the "{" and "}".
	 */
	private static final TokenPattern ANONYMOUS_CLASS_INIT_PATTERN = new TokenPattern()
			.sequence(NEW).repeated(EnumSet.of(DOT, IDENTIFIER, COLON))
			.skipNested(LT, GT, true).skipNested(LPAREN, RPAREN, true)
			.skipNested(LBRACE, RBRACE, false).group(0);

	/** The primitive types (i.e. keywords representing types) known to C#. */
	private static final EnumSet<ETokenType> PRIMITIVE_TYPES = EnumSet.of(BOOL,
			BYTE, CHAR, DECIMAL, DOUBLE, ENUM, FLOAT, INT, LONG, SBYTE, SHORT,
			UINT, ULONG, USHORT, STRING, OBJECT, DYNAMIC, VAR);

	/** The tokens that may appear inside a generic. */
	private static final EnumSet<ETokenType> GENERICS_CONTENT;
	static {
		GENERICS_CONTENT = EnumSet.of(IDENTIFIER, QUESTION, DOT, COMMA, SUPER,
				EXTENDS, AND, LBRACK, RBRACK, AT, COLON);
		GENERICS_CONTENT.addAll(PRIMITIVE_TYPES);
	}

	/** Matches a non-primitive type, possibly including generic expressions. */
	private static final TokenPattern GENERIC_TYPE = new TokenPattern()
			.sequence(IDENTIFIER)
			.repeated(COLON, COLON, IDENTIFIER)
			.repeated(
					new TokenPattern().repeated(DOT, IDENTIFIER).skipNested(LT,
							GT, false)).repeated(DOT, IDENTIFIER);

	/**
	 * Matches <code>out</code> and <code>ref</code> parameters in a formal
	 * parameter list. Group 0 contains the name of the parameter, group 1 will
	 * contain "out" or "ref".
	 */
	private static final TokenPattern OUT_REF_FORMAL_PARAMETER_PATTERN = new TokenPattern()
			.alternative(OUT, REF).group(1)
			.alternative(PRIMITIVE_TYPES, GENERIC_TYPE).skipTo(IDENTIFIER)
			.group(0);

	/** Constructor. */
	public CsDefUseHeuristic(IConQATLogger logger) {
		super(EnumSet.of(QUESTION, MULT, THIS, PARAMS, OUT, REF),
				PRIMITIVE_TYPES, GENERICS_CONTENT,
				CsConditionHeuristic.DEREFERENCE_OPERATORS,
				CsConditionHeuristic.NOT_ALLOWED_BEFORE_IDENTIFIER, logger);
	}

	/** {@inheritDoc} */
	@Override
	protected boolean skipAnnotations(TokenStreamParser parser) {
		while (parser.isAnyOf(EnumSet.of(LBRACK))) {
			if (!parser.skipBalanced(LBRACK, RBRACK,
					EnumSet.complementOf(EnumSet.of(LBRACK, RBRACK)))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Adds reads for variables that are used as functions.
	 */
	@Override
	protected List<String> parseReads(List<IToken> tokens) {
		List<String> reads = super.parseReads(tokens);
		List<TokenPatternMatch> matches = FUNCTOR_READ_PATTERN.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variable = match.groupString(0);
			if (isKnown(variable)) {
				reads.add(variable);
			}
		}
		return reads;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Also parses pointer dereferences and dereferences of functors.
	 */
	@Override
	protected List<String> parseDereferences(List<IToken> tokens) {
		List<String> dereferences = super.parseDereferences(tokens);
		List<TokenPatternMatch> matches = POINTER_OR_FUNCTOR_DEREFERENCE_PATTERN
				.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variable = match.groupString(0);
			if (isKnown(variable)) {
				dereferences.add(variable);
			}
		}
		return dereferences;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Also parses ref and out parameters in method calls and removes anonymous
	 * class initializations from the token stream.
	 */
	@Override
	protected List<String> parseAssignmentsAndDefinitions(List<IToken> tokens,
			VariableReadWriteInfo info) {
		tokens = filterAnonymousClassInits(tokens);
		List<String> writtenAndNotReadVariables = super
				.parseAssignmentsAndDefinitions(tokens, info);
		List<TokenPatternMatch> matches = OUT_REF_PARAMETER_PATTERN
				.match(tokens);
		for (TokenPatternMatch match : matches) {
			String variable = match.groupString(0);
			if (isKnown(variable)) {
				info.getAssignments().add(new VariableWrite(variable));
				if (match.groupTokens(1).get(0).getType() == OUT) {
					// out variables are not read by the called method but
					// always overwritten
					writtenAndNotReadVariables.add(variable);
				}
			}
		}
		return writtenAndNotReadVariables;
	}

	/**
	 * Removes the tokens belonging to member initialization in anonymous
	 * classes, e.g.
	 * 
	 * <pre>
	 * Foo a = new Foo() {
	 * 		b = 1,
	 * 		c = new Bar<Goo>(),
	 * };
	 * </pre>
	 */
	private List<IToken> filterAnonymousClassInits(List<IToken> tokens) {
		List<IToken> filteredTokens = new ArrayList<IToken>(tokens);
		List<TokenPatternMatch> matches = ANONYMOUS_CLASS_INIT_PATTERN
				.match(tokens);
		filteredTokens.removeAll(TokenPatternMatch.getAllTokens(matches, 0));
		return filteredTokens;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Makes the writes to out and ref parameters a default initialization.
	 */
	@Override
	public VariableReadWriteInfo parseParameterList(
			List<IToken> parameterListTokens, String methodName) {
		boolean mustContainTypes = methodName != null;
		VariableReadWriteInfo info = super.parseParameterList(
				parameterListTokens, methodName, mustContainTypes);
		List<TokenPatternMatch> matches = OUT_REF_FORMAL_PARAMETER_PATTERN
				.match(parameterListTokens);
		List<String> outRefParameters = TokenPatternMatch.getAllStrings(
				matches, 0);
		for (VariableWrite write : info.getDefinitions()) {
			if (outRefParameters.contains(write.getChangedVariable())) {
				write.makeDefaultInitialization();
			}
		}
		return info;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Makes sure that variables that have been defined in this statement are
	 * not dereferenced. Otherwise <code>int* f</code> would be parsed as a
	 * definition of f and a dereference.
	 */
	@Override
	public VariableReadWriteInfo parseStatement(List<IToken> tokens) {
		VariableReadWriteInfo info = super.parseStatement(tokens);
		for (VariableWrite write : info.getDefinitions()) {
			info.getDereferences().remove(write.getChangedVariable());
		}
		return info;
	}

}
