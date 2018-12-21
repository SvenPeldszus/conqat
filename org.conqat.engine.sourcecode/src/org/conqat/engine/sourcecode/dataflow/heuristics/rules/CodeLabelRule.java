/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CodeLabelRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.JumpLabelManager;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms labels that mark a statement. This creates two jump labels and two
 * nodes: one before the labeled statement and one after. The one before is used
 * for <code>goto</code> and <code>continue</code> statements, the one after is
 * used for <code>break</code> statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 98F68F79B71567B404E194C695BDD4AE
 */
public class CodeLabelRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		List<IToken> tokens = entity.ownStartTokens();

		String label = tokens.get(0).getText();
		ControlFlowNode beforeNode = context.createNode(tokens, false);
		context.getCodeLabelManager().addLabeledNode(label, beforeNode);

		if (entities.size() == 1) {
			// a label without a labeled node
			return new Result(1, beforeNode, Arrays.asList(beforeNode));
		}

		ControlFlowNode afterNode = context.createNode(tokens, false);
		context.getCodeLabelManager().addLabeledNode(
				JumpLabelManager.AFTER_JUMP_LABEL_PREFIX + label, afterNode);

		Result result = creator.transformOneStep(entities.subList(1,
				entities.size()));
		ControlFlowNode.link(beforeNode, result.getEntryNode());
		for (ControlFlowNode exitNode : result.getExitNodes()) {
			ControlFlowNode.link(exitNode, afterNode);
		}

		return new Result(1 + result.getNumberOfConsumedEntities(), beforeNode,
				Arrays.asList(afterNode));
	}

}