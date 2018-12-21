/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: UsingRule.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules;

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms using statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: AE70E5BFF08F03E0B0C40A5F4339339D
 */
public class UsingRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		List<IToken> tokens = entity.ownStartTokens();
		ETokenType[] types = { ETokenType.RPAREN };
		List<IToken> innerTokens = tokens.subList(2,
				TokenStreamUtils.findLast(tokens, types));

		context.getDefUseHeuristic().openNewScope();
		ControlFlowNode usingNode = context.createNode(innerTokens, false);
		Result result = creator.transform(entity.getChildren());
		context.getDefUseHeuristic().closeCurrentScope();

		ControlFlowNode closeNode = context.createSyntheticNode();
		for (VariableWrite write : usingNode.getReadWriteInfo()
				.getDefinitions()) {
			closeNode.getReadWriteInfo().getReads()
					.add(write.getChangedVariable());
		}
		for (VariableWrite write : usingNode.getReadWriteInfo()
				.getAssignments()) {
			closeNode.getReadWriteInfo().getReads()
					.add(write.getChangedVariable());
		}

		ControlFlowNode.link(usingNode, result.getEntryNode());
		for (ControlFlowNode exitNode : result.getExitNodes()) {
			ControlFlowNode.link(exitNode, closeNode);
		}

		return new Result(1, usingNode, Arrays.asList(closeNode));
	}

}