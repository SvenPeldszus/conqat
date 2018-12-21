/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapConditionHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import static org.conqat.lib.scanner.ETokenType.ASSIGNED;
import static org.conqat.lib.scanner.ETokenType.BOUND;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.INITIAL;
import static org.conqat.lib.scanner.ETokenType.IS;
import static org.conqat.lib.scanner.ETokenType.NOT;

import java.util.List;
import java.util.ListIterator;

import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.conqat.engine.sourcecode.dataflow.heuristics.ConditionHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.AbapPatterns;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;

/**
 * Extracts interesting condition information.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: A3678E1F08EC9EA0A42168FB6959D72E
 */
public class AbapConditionHeuristic extends ConditionHeuristicBase {

	/**
	 * Matches all ABAP null checks. Group 0 contains the variables that are
	 * checked for "== null" and group 1 those that are checked for "!= null".
	 */
	private static final TokenPattern NULL_CHECK_PATTERN = new TokenPattern()
			.alternative(
					// assigned for field symbols
					new TokenPattern()
							.sequence(AbapPatterns.FIELD_SYMBOL_PATTERN)
							.group(0).sequence(IS, NOT, ASSIGNED),
					new TokenPattern().sequence(NOT)
							.sequence(AbapPatterns.FIELD_SYMBOL_PATTERN)
							.group(0).sequence(IS, ASSIGNED),
					new TokenPattern()
							.sequence(AbapPatterns.FIELD_SYMBOL_PATTERN)
							.group(1).sequence(IS, ASSIGNED),
					// bound for references
					new TokenPattern().sequence(IDENTIFIER).group(0)
							.sequence(IS, NOT, BOUND),
					new TokenPattern().sequence(NOT).sequence(IDENTIFIER)
							.group(0).sequence(IS, BOUND),
					new TokenPattern().sequence(IDENTIFIER).group(1)
							.sequence(IS, BOUND),
					// initial for references
					new TokenPattern().sequence(IDENTIFIER).group(1)
							.sequence(IS, NOT, INITIAL), new TokenPattern()
							.sequence(NOT).sequence(IDENTIFIER).group(1)
							.sequence(IS, INITIAL), new TokenPattern()
							.sequence(IDENTIFIER).group(0)
							.sequence(IS, INITIAL));

	/**
	 * Constructor.
	 * 
	 * @param defUseHeuristic
	 *            may be <code>null</code> for testing purposes. In this case,
	 *            every variable is assumed to be known in the current scope.
	 */
	public AbapConditionHeuristic(IDefUseHeuristic defUseHeuristic) {
		super(defUseHeuristic, ETokenType.AND, ETokenType.OR);
	}

	/** {@inheritDoc} */
	@Override
	protected TokenPattern getNullCheckPattern() {
		return NULL_CHECK_PATTERN;
	}

	/** {@inheritDoc} */
	@Override
	protected List<IToken> filter(List<IToken> tokens) {
		List<IToken> filteredTokens = tokens;
		IToken lastToken = CollectionUtils.getLast(tokens);
		if (lastToken != null && lastToken.getType() == ETokenType.DOT) {
			filteredTokens = tokens.subList(0, tokens.size() - 1);
		}
		return super.filter(filteredTokens);
	}

	/** {@inheritDoc} */
	@Override
	protected List<String> extractVariableNamesFromChainMatch(
			TokenPatternMatch match, int groupNumber) {
		List<String> variables = super.extractVariableNamesFromChainMatch(
				match, groupNumber);
		for (ListIterator<String> iterator = variables.listIterator(); iterator
				.hasNext();) {
			String variable = iterator.next();
			if (variable.equals("<")) {
				iterator.remove();
				variable = iterator.next();
				iterator.set("<" + variable + ">");
			} else if (variable.equals(">")) {
				iterator.remove();
			}
		}
		return variables;
	}

}
