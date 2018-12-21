/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: FunctionLikeRule.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules;

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Transforms functions, e.g. reports and methods as well as event handlers.
 * Used to create an exit node for all child statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: 60AAD8598FEE8F83DBE45132FF71E849
 */
public class FunctionLikeRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity functionEntity = entities.get(0);
		Result result = creator.transform(functionEntity.getChildren());

		ControlFlowNode exitNode = context.createSyntheticNode();
		for (ControlFlowNode node : result.getExitNodes()) {
			ControlFlowNode.link(node, exitNode);
		}
		for (ControlFlowNode node : context.getReturnNodes()) {
			ControlFlowNode.link(node, exitNode);
		}
		// exit, check, etc. also return from the current method if used outside
		// a loop
		for (ControlFlowNode node : context.getBreakNodes()) {
			ControlFlowNode.link(node, exitNode);
		}

		return new Result(1, result.getEntryNode(), Arrays.asList(exitNode));
	}

}