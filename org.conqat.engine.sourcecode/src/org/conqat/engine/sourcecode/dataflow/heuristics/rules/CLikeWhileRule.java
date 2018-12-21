/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CLikeWhileRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import static org.conqat.lib.scanner.ETokenType.BOOLEAN_LITERAL;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.util.List;

import org.conqat.lib.scanner.IToken;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.WhileRuleBase;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;

/**
 * Rule for C-like while loops.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 968F41294157F03C6904F179E0786DF8
 */
public class CLikeWhileRule extends WhileRuleBase {

	/** Matches a "while(true)" loop. */
	private static final TokenPattern WHILE_TRUE_PATTERN = new TokenPattern()
			.sequence(WHILE, LPAREN, BOOLEAN_LITERAL, RPAREN);

	/**
	 * {@inheritDoc}
	 * 
	 * Detects simple <code>while (true)</code> loops.
	 */
	@Override
	protected boolean isInfiniteLoop(List<IToken> tokens) {
		return WHILE_TRUE_PATTERN.matches(tokens);
	}

}
