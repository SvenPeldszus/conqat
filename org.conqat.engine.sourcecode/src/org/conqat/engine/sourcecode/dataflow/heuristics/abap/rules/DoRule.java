/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DoRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules;

import java.util.List;

import org.conqat.engine.sourcecode.dataflow.heuristics.rules.WhileRuleBase;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms do statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 6398E2E3C8E3B0D76E851E87367D75B2
 */
public class DoRule extends WhileRuleBase {

	/** {@inheritDoc} */
	@Override
	protected boolean isInfiniteLoop(List<IToken> tokens) {
		if (tokens.size() < 3) {
			// does not have the "n times" addition
			return true;
		}
		return false;
	}

}