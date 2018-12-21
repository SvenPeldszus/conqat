/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: LoopRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules;

import java.util.List;

import org.conqat.engine.sourcecode.dataflow.heuristics.rules.WhileRuleBase;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms all loops, except do loops.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 72DCFEF8B3F4C6654E8820D2CA5A6B41
 */
public class LoopRule extends WhileRuleBase {

	/**
	 * {@inheritDoc}
	 * 
	 * The <code>do</code> loop provides the simplest infinite loop, so it is
	 * unlikely that any of the other loop constructs are used to create
	 * infinite loops. This method therefore simply returns <code>false</code>.
	 */
	@Override
	protected boolean isInfiniteLoop(List<IToken> tokens) {
		return false;
	}

}