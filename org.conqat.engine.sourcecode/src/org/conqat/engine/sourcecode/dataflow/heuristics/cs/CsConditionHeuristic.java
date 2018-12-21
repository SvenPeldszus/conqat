/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsConditionHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import static org.conqat.lib.scanner.ETokenType.ARROW;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IS;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.lib.scanner.ETokenType;

/**
 * Adds some C# specific idioms to condition parsing.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: D5E0D0BE9733A2047AFCBC57F5F4A4DB
 */
public class CsConditionHeuristic extends CLikeConditionHeuristic {

	/**
	 * The operators that may be used to dereference an object reference or
	 * pointer.
	 */
	public static final EnumSet<ETokenType> DEREFERENCE_OPERATORS = EnumSet.of(
			DOT, ARROW, LBRACK, LPAREN);

	/**
	 * The tokens that are not allowed to appear before an identifier.
	 */
	public static final EnumSet<ETokenType> NOT_ALLOWED_BEFORE_IDENTIFIER = EnumSet
			.of(DOT, ARROW);

	/** Matches an "x is Type" check. Group 2 contains the checked identifier. */
	private static final TokenPattern IS_CHECK_PATTERN = new TokenPattern()
			.notPrecededBy(NOT_ALLOWED_BEFORE_IDENTIFIER).sequence(IDENTIFIER)
			.group(2).sequence(IS, IDENTIFIER)
			.repeated(EnumSet.of(DOT, IDENTIFIER, COLON));

	/**
	 * Constructor.
	 * 
	 * @param defUseHeuristic
	 *            may be <code>null</code> for testing purposes. In this case,
	 *            every variable is assumed to be known in the current scope.
	 */
	public CsConditionHeuristic(IDefUseHeuristic defUseHeuristic) {
		super(defUseHeuristic, DEREFERENCE_OPERATORS,
				NOT_ALLOWED_BEFORE_IDENTIFIER);
	}

	/** {@inheritDoc} */
	@Override
	protected TokenPattern getNullCheckPattern() {
		TokenPattern clikePattern = super.getNullCheckPattern();
		return new TokenPattern().alternative(clikePattern, IS_CHECK_PATTERN);
	}

}
