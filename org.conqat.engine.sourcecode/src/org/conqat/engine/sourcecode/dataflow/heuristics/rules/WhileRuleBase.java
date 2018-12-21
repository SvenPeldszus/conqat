/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: WhileRuleBase.java 51147 2014-11-14 10:14:06Z streitel $            
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
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms while statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: F5FF557944F4D31D4F9175800599FF21
 */
public abstract class WhileRuleBase extends LoopRuleBase {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		UnmodifiableList<IToken> tokens = entities.get(0).ownStartTokens();
		ControlFlowNode whileNode = context.createNode(tokens, true);

		List<ShallowEntity> childNodes = entities.get(0).getChildren();
		Result body = transformChildrenWithNewLoopContext(context, creator,
				childNodes);
		ControlFlowNode.link(whileNode, body.getEntryNode());
		for (ControlFlowNode exitNode : body.getExitNodes()) {
			ControlFlowNode.link(exitNode, whileNode);
		}

		List<ControlFlowNode> exitNodes = handleContinueAndBreak(context,
				whileNode);
		if (!isInfiniteLoop(tokens)) {
			exitNodes.add(whileNode);
		}

		return new Result(1, whileNode, exitNodes);
	}

	/**
	 * Returns <code>true</code> if the loop that consists of the given tokens
	 * is an infinite loop.
	 */
	protected abstract boolean isInfiniteLoop(List<IToken> tokens);

}