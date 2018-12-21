/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: BreakRule.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.JumpLabelManager;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Handles <code>break</code> statements, including "break to label".
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: EA5EB86C9275D7CEF4FFD4A9843C5B71
 */
public class BreakRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		List<IToken> tokens = entities.get(0).ownStartTokens();
		ControlFlowNode node = context.createNode(tokens, false);

		String label = extractBreakLabel(tokens);
		if (label == null) {
			context.getBreakNodes().add(node);
		} else {
			context.getCodeLabelManager().addJumpNode(node,
					JumpLabelManager.AFTER_JUMP_LABEL_PREFIX + label);
		}
		return new Result(1, node,
				CollectionUtils.<ControlFlowNode> emptyList());
	}

	/**
	 * Returns the label to which the break node will jump or <code>null</code>
	 * if there is no label.
	 */
	private String extractBreakLabel(List<IToken> tokens) {
		if (tokens.size() > 1
				&& tokens.get(1).getType() == ETokenType.IDENTIFIER) {
			return tokens.get(1).getText();
		}
		return null;
	}

}