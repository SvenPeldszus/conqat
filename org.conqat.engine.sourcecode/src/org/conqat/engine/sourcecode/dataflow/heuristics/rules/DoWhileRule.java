/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DoWhileRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.LoopRuleBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Transforms do...while statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 1181BE6162721C1AB369D08FD2746DEA
 */
public class DoWhileRule extends LoopRuleBase {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ControlFlowNode doWhileNode = context.createNode(entities.get(0)
				.ownEndTokens(), true);

		Result body = transformChildrenWithNewLoopContext(context, creator,
				entities.get(0).getChildren());
		ControlFlowNode.link(doWhileNode, body.getEntryNode());
		for (ControlFlowNode exitNode : body.getExitNodes()) {
			ControlFlowNode.link(exitNode, doWhileNode);
		}

		List<ControlFlowNode> exitNodes = handleContinueAndBreak(context,
				doWhileNode);
		exitNodes.add(doWhileNode);

		return new Result(1, body.getEntryNode(), exitNodes);
	}

}