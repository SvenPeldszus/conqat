/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: MethodRuleBase.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.IToken;

/**
 * Base class that handles common tasks for rules that process function-like
 * entities.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: A527E52489C14F24E6E6EDAEEDC9AB09
 */
public abstract class MethodRuleBase implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity methodEntity = entities.get(0);
		List<IToken> parameterListTokens = extractParameterListTokens(methodEntity);
		VariableReadWriteInfo parameterInfo = context
				.getDefUseHeuristic()
				.parseParameterList(parameterListTokens, methodEntity.getName());
		ControlFlowNode parameterNode = context.createNode(parameterListTokens,
				parameterInfo);

		ControlFlowNode exitNode = linkNodes(context, creator, methodEntity,
				parameterNode);
		return new Result(1, parameterNode, Arrays.asList(exitNode));
	}

	/**
	 * Returns the tokens that make up the parameter list of the given method
	 * entity, i.e. everything inside the "(" and ")" tokens.
	 */
	protected abstract List<IToken> extractParameterListTokens(
			ShallowEntity methodEntity);

	/**
	 * Links the the parameter node to the method body and the method body to
	 * the synthetic exit node. Also links all label jumps.
	 */
	private ControlFlowNode linkNodes(Context context,
			ControlFlowCreator creator, ShallowEntity methodEntity,
			ControlFlowNode parameterNode) {
		Result result = RuleUtils.transformInNewScope(creator, context,
				methodEntity.getChildren());

		ControlFlowNode.link(parameterNode, result.getEntryNode());
		ControlFlowNode exitNode = context.createSyntheticNode();
		for (ControlFlowNode node : result.getExitNodes()) {
			ControlFlowNode.link(node, exitNode);
		}
		for (ControlFlowNode node : context.getReturnNodes()) {
			ControlFlowNode.link(node, exitNode);
		}

		context.getCodeLabelManager().connectAllNodesAndClear();
		return exitNode;
	}

}