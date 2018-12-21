/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: SwitchRule.java 51147 2014-11-14 10:14:06Z streitel $            
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
 * Transforms a switch statement.
 * 
 * Assumes the shallow entities are ordered like this:
 * 
 * <pre>
 * switch entity
 *   case entity
 *   statement
 *   statement
 *   statement
 *   case entity
 *   statement
 *   statement
 *   ...
 * </pre>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 4A8DE675B0F6E4554F52ACC48B5CD98B
 */
public class SwitchRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity switchEntity = entities.get(0);
		ControlFlowNode switchNode = context.createNode(
				switchEntity.ownStartTokens(), false);
		List<ControlFlowNode> exitNodes = new ArrayList<ControlFlowNode>();
		context.saveCurrentSwitchNodes();
		context.saveCurrentBreakNodes();

		// we ignore the result of the transformation as the relevant
		// information is communicated via the context.
		RuleUtils.transformInNewScope(creator, context,
				switchEntity.getChildren());
		for (ControlFlowNode entryNode : context.getCaseEntryNodes()) {
			ControlFlowNode.link(switchNode, entryNode);
		}

		exitNodes.addAll(context.getBreakNodes());
		context.getBreakNodes().clear();

		if (context.getDefaultCaseEntryNode() == null) {
			exitNodes.add(switchNode);
		} else {
			ControlFlowNode.link(switchNode, context.getDefaultCaseEntryNode());
		}

		context.getCaseLabelManager().connectAllNodesAndClear();
		context.restorePreviousSwitchNodes();
		context.restorePreviousBreakNodes();
		return new Result(1, switchNode, exitNodes);
	}

}