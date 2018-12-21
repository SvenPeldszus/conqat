/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CLikeConditionHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.clike;

import static org.conqat.lib.scanner.ETokenType.EQEQ;
import static org.conqat.lib.scanner.ETokenType.NOTEQ;
import static org.conqat.lib.scanner.ETokenType.NULL_LITERAL;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.dataflow.heuristics.ConditionHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;

/**
 * Extracts interesting condition information for C-like languages.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 5BE061A3D61B150ED52A30175AC3157A
 */
public class CLikeConditionHeuristic extends ConditionHeuristicBase {

	/**
	 * The operators that may be used to dereference a variable, e.g. ".".
	 */
	private final EnumSet<ETokenType> dereferenceOperators;

	/**
	 * Tokens that are not allowed before an identifier.
	 */
	private final EnumSet<ETokenType> notAllowedBeforeIdentifier;

	/**
	 * Constructor.
	 * 
	 * @param dereferenceOperators
	 *            the operators that may be used to dereference a variable, e.g.
	 *            ".".
	 * @param notAllowedBeforeIdentifier
	 *            tokens that are not allowed before an identifier.
	 */
	public CLikeConditionHeuristic(IDefUseHeuristic defUseHeuristic,
			EnumSet<ETokenType> dereferenceOperators,
			EnumSet<ETokenType> notAllowedBeforeIdentifier) {
		super(defUseHeuristic, ETokenType.ANDAND, ETokenType.OROR);
		this.dereferenceOperators = dereferenceOperators;
		this.notAllowedBeforeIdentifier = notAllowedBeforeIdentifier;
	}

	/** {@inheritDoc} */
	@Override
	protected TokenPattern getNullCheckPattern() {
		return new TokenPattern().alternative(
				new TokenPattern().notPrecededBy(notAllowedBeforeIdentifier)
						.alternative(
								new TokenPattern()
										.sequence(ETokenClass.IDENTIFIER)
										.group(0).sequence(EQEQ)
										.sequence(NULL_LITERAL),
								new TokenPattern()
										.sequence(ETokenClass.IDENTIFIER)
										.group(1).sequence(NOTEQ)
										.sequence(NULL_LITERAL)),
				new TokenPattern().alternative(
						new TokenPattern().sequence(NULL_LITERAL)
								.sequence(EQEQ)
								.sequence(ETokenClass.IDENTIFIER).group(0),
						new TokenPattern().sequence(NULL_LITERAL)
								.sequence(NOTEQ)
								.sequence(ETokenClass.IDENTIFIER).group(1))
						.notFollowedBy(dereferenceOperators));
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Additionally parses instanceof expressions.
	 */
	@Override
	protected void parseSinglePattern(TokenPatternMatch singleMatch,
			Condition condition) {
		super.parseSinglePattern(singleMatch, condition);
		parseInstanceOfPattern(singleMatch, condition);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * Additionally parses instanceof expressions.
	 */
	@Override
	protected void parseAndPattern(TokenPatternMatch andMatch,
			Condition condition) {
		super.parseAndPattern(andMatch, condition);
		parseInstanceOfPattern(andMatch, condition);
	}

	/**
	 * Parses the instanceof expressions this heuristic supports.
	 */
	private void parseInstanceOfPattern(TokenPatternMatch andMatch,
			Condition condition) {
		List<String> instanceofVariables = andMatch.groupTexts(2);
		for (String variable : instanceofVariables) {
			if (isKnown(variable)) {
				condition.getYesBranchInfo().add(variable, false);
			}
		}
	}

}
