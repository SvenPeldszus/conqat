/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ForRule.java 51707 2015-02-08 16:28:18Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.rules;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.utils.DataflowExceptionUtils;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for "for" and "foreach" control flow rules.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51707 $
 * @ConQAT.Rating YELLOW Hash: EF9563FC1BF974F795A95EC6EF5A1F73
 */
public class ForRule extends LoopRuleBase {

	/**
	 * Matches the last identifier in a loop condition. Group 0 contains the
	 * matched identifier.
	 */
	private static final TokenPattern LAST_IDENTIFIER_IN_LOOP_PATTERN = new TokenPattern()
			.beginningOfStream().sequence(ETokenClass.IDENTIFIER).group(0)
			.sequence(ETokenType.RPAREN);

	/** The token that separates the two halves of a for-each loop. */
	private final ETokenType forEachSeparatorToken;

	/** Constructor. */
	public ForRule(ETokenType forEachSeparatorToken) {
		this.forEachSeparatorToken = forEachSeparatorToken;
	}

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		List<IToken> tokens = entities.get(0).ownStartTokens();
		List<List<IToken>> parts = TokenStreamUtils.split(tokens,
				ETokenType.SEMICOLON);
		context.getDefUseHeuristic().openNewScope();

		ControlFlowNode initNode;
		ControlFlowNode conditionEntryNode;
		ControlFlowNode conditionExitNode;
		ControlFlowNode continueNode;
		if (parts.size() == 1) {
			// foreach loop
			parts = TokenStreamUtils.split(tokens,
					EnumSet.of(forEachSeparatorToken), 2);
			CCSMAssert.isTrue(parts.size() == 2,
					DataflowExceptionUtils.createMessage(
							"Found a for-each loop that does not have "
									+ "two parts", tokens));

			List<IToken> leftSide = parts.get(0);
			IToken loopVariableToken = CollectionUtils.getLast(leftSide);
			// C# allows some keywords to be used as variable names!
			CCSMAssert
					.isTrue(loopVariableToken.getType() == ETokenType.IDENTIFIER
							|| loopVariableToken.getType().getTokenClass() == ETokenClass.KEYWORD,
							DataflowExceptionUtils.createMessage(
									"Found a for-each loop that does not have "
											+ "a proper loop variable", tokens));

			VariableReadWriteInfo assignmentInfo = new VariableReadWriteInfo();
			if (loopVariableToken.getType() == ETokenType.IDENTIFIER) {
				// we do not want to record an assignment for a keyword variable
				// as no read will ever be registered for it
				String loopVariable = loopVariableToken.getText();
				context.getDefUseHeuristic().addToScope(loopVariable);
				assignmentInfo.getDefinitions().add(
						new VariableWrite(loopVariable));
			}

			List<IToken> rightSide = parts.get(1);
			// the actual variables are read only once in the initialization
			initNode = context.createNode(rightSide, false);
			conditionEntryNode = context.createSyntheticNode();

			// after the condition, the loop variable is assigned and the
			// collection dereferenced
			TokenPatternMatch match = LAST_IDENTIFIER_IN_LOOP_PATTERN
					.matchFirst(rightSide);
			if (match != null) {
				// add a dereference for the collection
				assignmentInfo.getDereferences().add(match.groupString(0));
			}
			conditionExitNode = context.createNode(leftSide, assignmentInfo);
			continueNode = conditionEntryNode;

			ControlFlowNode.link(conditionEntryNode, conditionExitNode);
		} else {
			// for (a; b; c)
			List<IToken> initPart = parts.get(0);

			initNode = context.createNode(initPart.subList(2, initPart.size()),
					false);

			conditionEntryNode = context.createNode(parts.get(1), true);
			conditionExitNode = conditionEntryNode;

			continueNode = context.createNode(parts.get(2), false);

			ControlFlowNode.link(continueNode, conditionEntryNode);
		}

		ControlFlowNode.link(initNode, conditionEntryNode);

		Result body = transformChildrenWithNewLoopContext(context, creator,
				entities.get(0).getChildren());

		ControlFlowNode.link(conditionExitNode, body.getEntryNode());
		for (ControlFlowNode exitNode : body.getExitNodes()) {
			ControlFlowNode.link(exitNode, continueNode);
		}

		List<ControlFlowNode> exitNodes = handleContinueAndBreak(context,
				continueNode);
		exitNodes.add(conditionEntryNode);

		context.getDefUseHeuristic().closeCurrentScope();
		return new Result(1, initNode, exitNodes);
	}

}