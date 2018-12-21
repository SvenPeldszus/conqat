/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: GotoRule.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules;

import static org.conqat.lib.scanner.ETokenType.CASE;
import static org.conqat.lib.scanner.ETokenType.DEFAULT;
import static org.conqat.lib.scanner.ETokenType.GOTO;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms using statements.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51545 $
 * @ConQAT.Rating YELLOW Hash: 2246B3EADE79C3F2E24F03A26D6C2534
 */
public class GotoRule implements IControlFlowRule {

	/** Matches a goto statement. Group 0 contains the jump label. */
	private static final TokenPattern GOTO_PATTERN = new TokenPattern()
			.beginningOfStream()
			.sequence(GOTO)
			.alternative(
					new TokenPattern().alternative(DEFAULT, IDENTIFIER)
							.group(0),
					new TokenPattern()
							.sequence(CASE)
							.repeated(
									EnumSet.complementOf(EnumSet.of(SEMICOLON)))
							.group(0));

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity entity = entities.get(0);
		List<IToken> tokens = entity.ownStartTokens();

		ControlFlowNode node = context.createNode(tokens, false);
		String label = extractLabel(tokens);
		if (TokenStreamUtils.containsAny(tokens, CASE, DEFAULT)) {
			context.getCaseLabelManager().addJumpNode(node, label);
		} else {
			context.getCodeLabelManager().addJumpNode(node, label);
		}

		return new Result(1, node,
				CollectionUtils.<ControlFlowNode> emptyList());
	}

	/**
	 * Returns the label to which the goto is trying to jump.
	 */
	private String extractLabel(List<IToken> tokens) {
		TokenPatternMatch match = GOTO_PATTERN.matchFirst(tokens);
		CCSMAssert.isNotNull(match, DataflowExceptionUtils.createMessage(
				"Found malformed goto", tokens));
		return match.groupString(0);
	}

}