/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ConditionHeuristicBase.java 51696 2015-02-06 14:01:51Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RPAREN;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Takes care of most of the condition parsing for non-shortcurcuit conditions.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51696 $
 * @ConQAT.Rating YELLOW Hash: 4393BB8DDEE1AE01BD540CB0CDE57A6D
 */
public abstract class ConditionHeuristicBase {

	/**
	 * Matches tokens that are not interesting to the condition analysis that
	 * occur after the actual condition, e.g. ");" or ") } #IFDEF foo". Group 0
	 * contains all the uninteresting tokens.
	 */
	private static final TokenPattern UNINTERESTING_SUFFIX_PATTERN = new TokenPattern()
			.sequence(
					new TokenPattern().optional(RPAREN).optional(LBRACE)
							.optional(ETokenType.SEMICOLON).optional(RBRACE)
							.repeated(ETokenType.PREPROCESSOR_DIRECTIVE)
							.endOfStream()).group(0);

	/**
	 * Matches tokens that are not interesting to the condition analysis that
	 * occur before the actual condition, e.g. "if (" or "} while". Group 0
	 * contains all the uninteresting tokens.
	 */
	private static final TokenPattern UNINTERESTING_PREFIX_PATTERN = new TokenPattern()
			.sequence(
					new TokenPattern().beginningOfStream().optional(RBRACE)
							.repeated(ETokenClass.KEYWORD).optional(LPAREN))
			.group(0);

	/** The def use heuristic. */
	protected final IDefUseHeuristic defUseHeuristic;

	/** The logical and operator. */
	private final ETokenType andOperator;

	/** The logical or operator. */
	private final ETokenType orOperator;

	/**
	 * Constructor.
	 */
	public ConditionHeuristicBase(IDefUseHeuristic defUseHeuristic,
			ETokenType andOperator, ETokenType orOperator) {
		this.andOperator = andOperator;
		this.orOperator = orOperator;
		this.defUseHeuristic = defUseHeuristic;
	}

	/**
	 * Returns a pattern that identifies all null checks of single variables in
	 * the conditional. The pattern must put the name of the null-checked
	 * identifier into group 0 if it will be null if the check succeeds and into
	 * group 1 if it will be non-null if the check succeeds.
	 */
	protected abstract TokenPattern getNullCheckPattern();

	/**
	 * Creates a pattern that parses condition chains using the given boolean
	 * operator, e.g. "something && something && something"
	 */
	private TokenPattern createChainPattern(ETokenType booleanOperator) {
		TokenPattern unparsableConditionTerm = new TokenPattern()
				.repeated(EnumSet.complementOf(EnumSet.of(andOperator,
						orOperator, LBRACE)));
		return new TokenPattern()
				.beginningOfStream()
				.repeated(LPAREN)
				.alternative(getNullCheckPattern(), unparsableConditionTerm)
				.repeated(RPAREN)
				.repeated(
						new TokenPattern()
								.sequence(booleanOperator)
								.repeated(LPAREN)
								.alternative(getNullCheckPattern(),
										unparsableConditionTerm)
								.repeated(RPAREN)).endOfStream();
	}

	/**
	 * If the given node represents a conditional, creates the {@link Condition}
	 * for it and stores it in the node.
	 */
	public void createCondition(ControlFlowNode node) {
		List<IToken> tokens = node.getTokens();
		Condition condition = parseCondition(tokens);
		node.makeConditional(condition);
	}

	/**
	 * Parses the given tokens, which must belong to a condition statement, e.g.
	 * an "if".
	 */
	public Condition parseCondition(List<IToken> tokens) {
		Condition condition = new Condition();

		tokens = filter(tokens);
		TokenPatternMatch singleMatch = new TokenPattern().beginningOfStream()
				.repeated(LPAREN).sequence(getNullCheckPattern())
				.repeated(RPAREN).endOfStream().matchFirst(tokens);
		TokenPatternMatch andMatch = createChainPattern(andOperator)
				.matchFirst(tokens);
		TokenPatternMatch orMatch = createChainPattern(orOperator).matchFirst(
				tokens);

		if (singleMatch != null) {
			parseSinglePattern(singleMatch, condition);
		} else if (andMatch != null) {
			parseAndPattern(andMatch, condition);
		} else if (orMatch != null) {
			parseOrPattern(orMatch, condition);
		}

		List<TokenPatternMatch> checkMatches = getNullCheckPattern().match(
				tokens);
		parseCheckedVariables(checkMatches, condition);
		return condition;
	}

	/**
	 * Removes surplus tokens that are not interesting for the conditional.
	 */
	protected List<IToken> filter(List<IToken> tokens) {
		TokenPatternMatch preMatch = UNINTERESTING_PREFIX_PATTERN
				.matchFirst(tokens);
		TokenPatternMatch postMatch = UNINTERESTING_SUFFIX_PATTERN
				.matchFirst(tokens);
		int start = 0;
		if (preMatch != null) {
			start = preMatch.groupTokens(0).size();
		}
		int end = tokens.size();
		if (postMatch != null) {
			end = tokens.size() - postMatch.groupTokens(0).size();
		}
		return tokens.subList(start, end);
	}

	/**
	 * Parses the match of the single-null-check pattern.
	 */
	protected void parseSinglePattern(TokenPatternMatch singleMatch,
			Condition condition) {
		String eqeqVariable = singleMatch.groupString(0);
		String noteqVariable = singleMatch.groupString(1);
		if (!StringUtils.isEmpty(noteqVariable)) {
			if (isKnown(noteqVariable)) {
				condition.getYesBranchInfo().add(noteqVariable, false);
				condition.getNoBranchInfo().add(noteqVariable, true);
			}
		} else if (!StringUtils.isEmpty(eqeqVariable)) {
			if (isKnown(eqeqVariable)) {
				condition.getYesBranchInfo().add(eqeqVariable, true);
				condition.getNoBranchInfo().add(eqeqVariable, false);
			}
		}
	}

	/**
	 * Extracts all null-checked variables from the condition.
	 */
	private void parseCheckedVariables(List<TokenPatternMatch> checkMatches,
			Condition condition) {
		for (TokenPatternMatch match : checkMatches) {
			String eqeqVariable = match.groupString(0);
			String noteqVariable = match.groupString(1);
			if (!StringUtils.isEmpty(eqeqVariable)) {
				if (isKnown(eqeqVariable)) {
					condition.getNullCheckedVariables().add(eqeqVariable);
				}
			} else if (!StringUtils.isEmpty(noteqVariable)) {
				if (isKnown(noteqVariable)) {
					condition.getNullCheckedVariables().add(noteqVariable);
				}
			}
		}
	}

	/**
	 * Parses a match of the or-chain pattern.
	 */
	protected void parseOrPattern(TokenPatternMatch orMatch, Condition condition) {
		List<String> eqeqVariables = extractVariableNamesFromChainMatch(
				orMatch, 0);
		for (String variable : eqeqVariables) {
			if (isKnown(variable)) {
				condition.getNoBranchInfo().add(variable, false);
			}
		}
		List<String> noteqVariables = extractVariableNamesFromChainMatch(
				orMatch, 1);
		for (String variable : noteqVariables) {
			if (isKnown(variable)) {
				condition.getNoBranchInfo().add(variable, true);
			}
		}
	}

	/**
	 * Returns <code>true</code> if the given variable name is known in the
	 * current scope.
	 */
	protected boolean isKnown(String variable) {
		if (defUseHeuristic == null) {
			return true;
		}
		return defUseHeuristic.isKnown(variable);
	}

	/**
	 * Returns the names of all variables matched by a chain pattern into the
	 * given match.
	 */
	protected List<String> extractVariableNamesFromChainMatch(
			TokenPatternMatch match, int groupNumber) {
		return match.groupTexts(groupNumber);
	}

	/**
	 * Parses a match of the and-chain pattern.
	 */
	protected void parseAndPattern(TokenPatternMatch andMatch,
			Condition condition) {
		List<String> eqeqVariables = extractVariableNamesFromChainMatch(
				andMatch, 0);
		for (String variable : eqeqVariables) {
			if (isKnown(variable)) {
				condition.getYesBranchInfo().add(variable, true);
			}
		}
		List<String> noteqVariables = extractVariableNamesFromChainMatch(
				andMatch, 1);
		for (String variable : noteqVariables) {
			if (isKnown(variable)) {
				condition.getYesBranchInfo().add(variable, false);
			}
		}
	}

}
