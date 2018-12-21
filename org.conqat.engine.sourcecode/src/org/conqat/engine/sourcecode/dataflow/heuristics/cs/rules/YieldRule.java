/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: YieldRule.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms yield statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51545 $
 * @ConQAT.Rating YELLOW Hash: E8CCACD0F94BFF067C62300AEB5FBA06
 */
public class YieldRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		List<IToken> tokens = entity.ownStartTokens();
		boolean isYieldBreak = TokenStreamUtils.containsAny(tokens,
				ETokenType.BREAK);

		ControlFlowNode node = context.createNode(tokens, false);
		List<ControlFlowNode> exitNodes = new ArrayList<ControlFlowNode>();
		if (isYieldBreak) {
			context.getReturnNodes().add(node);
		} else {
			exitNodes.add(node);
		}

		return new Result(1, node, exitNodes);
	}

}