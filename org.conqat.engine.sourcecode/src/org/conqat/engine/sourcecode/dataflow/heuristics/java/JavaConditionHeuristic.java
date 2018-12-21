/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaConditionHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.INSTANCEOF;
import static org.conqat.lib.scanner.ETokenType.LBRACK;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.lib.scanner.ETokenType;

/**
 * Adds some Java specific idioms to condition parsing.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 681957A4B75DDF7F248DDA68FBB86234
 */
public class JavaConditionHeuristic extends CLikeConditionHeuristic {

	/**
	 * The operators that may be used to dereference an object reference.
	 */
	public static final EnumSet<ETokenType> DEREFERENCE_OPERATORS = EnumSet.of(
			DOT, LBRACK);

	/**
	 * The tokens that are not allowed to appear before an identifier.
	 */
	public static final EnumSet<ETokenType> NOT_ALLOWED_BEFORE_IDENTIFIER = EnumSet
			.of(DOT);

	/** Matches an instanceof check. Group 2 contains the checked variable. */
	private static final TokenPattern INSTANCEOF_PATTERN = new TokenPattern()
			.notPrecededBy(NOT_ALLOWED_BEFORE_IDENTIFIER).sequence(IDENTIFIER)
			.group(2).sequence(INSTANCEOF, IDENTIFIER)
			.repeated(DOT, IDENTIFIER);

	/**
	 * Constructor.
	 * 
	 * @param defUseHeuristic
	 *            may be <code>null</code> for testing purposes. In this case,
	 *            every variable is assumed to be known in the current scope.
	 */
	public JavaConditionHeuristic(IDefUseHeuristic defUseHeuristic) {
		super(defUseHeuristic, DEREFERENCE_OPERATORS,
				NOT_ALLOWED_BEFORE_IDENTIFIER);
	}

	/** {@inheritDoc} */
	@Override
	protected TokenPattern getNullCheckPattern() {
		TokenPattern clikePattern = super.getNullCheckPattern();
		return new TokenPattern().alternative(clikePattern, INSTANCEOF_PATTERN);
	}

}
