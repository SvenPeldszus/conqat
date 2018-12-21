/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AnonymousBlockRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Transforms anonymous blocks, e.g:
 * 
 * <pre>
 * {
 * 	foo.bar();
 * }
 * </pre>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: A060BCDE4D53FF5F5A9FCBAEB356D71B
 */
public class AnonymousBlockRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		ControlFlowNode blockNode = context.createNode(entity.ownStartTokens(),
				false);
		Result result = RuleUtils.transformInNewScope(creator, context,
				entity.getChildren());
		ControlFlowNode.link(blockNode, result.getEntryNode());
		return new Result(1, blockNode, result.getExitNodes());
	}

}