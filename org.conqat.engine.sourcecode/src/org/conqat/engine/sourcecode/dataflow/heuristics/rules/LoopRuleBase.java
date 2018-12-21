/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: LoopRuleBase.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.RuleUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Handles break and continue statements for loops.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 35E7007FC82AEDCAF020625CACC7906E
 */
public abstract class LoopRuleBase
		implements IControlFlowRule {

	/**
	 * Handles continue and break statements.
	 * 
	 * @param context
	 *            the context.
	 * @param continueTarget
	 *            the node to which continue statements should point.
	 * @return all break nodes that should be returned as exit nodes.
	 */
	protected List<ControlFlowNode> handleContinueAndBreak(Context context,
			ControlFlowNode continueTarget) {
		List<ControlFlowNode> exitNodes = new ArrayList<ControlFlowNode>();
		exitNodes.addAll(context.getBreakNodes());

		for (ControlFlowNode continueNode : context.getContinueNodes()) {
			ControlFlowNode.link(continueNode, continueTarget);
		}

		context.restorePreviousLoopNodes();
		return exitNodes;
	}

	/**
	 * Transforms the given child nodes in a modified context to ensure that
	 * continue and break statements are properly forwarded to this loop.
	 * 
	 * NOTE: you must call
	 * {@link #handleContinueAndBreak(Context, ControlFlowNode)}
	 * after calling this method and before transforming any other nodes.
	 * Otherwise, the continue and break statements might point to the wrong
	 * loop.
	 */
	protected Result transformChildrenWithNewLoopContext(Context context,
			ControlFlowCreator creator, List<ShallowEntity> childNodes) {
		context.saveCurrentLoopNodes();
		return RuleUtils.transformInNewScope(creator, context, childNodes);
	}

}