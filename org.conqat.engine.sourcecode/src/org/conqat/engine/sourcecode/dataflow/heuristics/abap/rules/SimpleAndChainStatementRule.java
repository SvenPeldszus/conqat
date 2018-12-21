/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: SimpleAndChainStatementRule.java 51543 2015-01-19 09:01:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap.rules;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms simple statements. This rule will gobble up all
 * {@link EShallowEntityType#STATEMENT} entities, so more specialized rules that
 * target special statements must be run before it.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51543 $
 * @ConQAT.Rating YELLOW Hash: 94A2CBB167AF551FFC6A705E780E416F
 */
public class SimpleAndChainStatementRule implements IControlFlowRule {

	/** {@inheritDoc} */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		ShallowEntity statementEntity = entities.get(0);
		List<List<IToken>> statements = splitCompoundStatements(statementEntity
				.ownStartTokens());

		ControlFlowNode firstNode = null;
		ControlFlowNode lastNode = null;
		for (List<IToken> statementTokens : statements) {
			ControlFlowNode statementNode = context.createNode(statementTokens,
					false);
			if (firstNode == null) {
				firstNode = statementNode;
			}
			if (lastNode != null) {
				ControlFlowNode.link(lastNode, statementNode);
			}
			lastNode = statementNode;
		}
		return new Result(1, firstNode, Arrays.asList(lastNode));
	}

	/**
	 * Splits a compound statement into several separate statements, e.g.
	 * "write: 'a', 'b'." will become "write 'a'" and "write 'b'.".
	 * 
	 * Note that only the last statement will include the final dot.
	 */
	private List<List<IToken>> splitCompoundStatements(
			List<IToken> ownStartTokens) {
		ETokenType[] types = { ETokenType.COLON };
		int colonIndex = TokenStreamUtils.find(ownStartTokens, types);
		if (colonIndex == -1) {
			return Collections.singletonList(ownStartTokens);
		}

		List<IToken> tokensBeforeColon = ownStartTokens.subList(0, colonIndex);
		List<IToken> tokensAfterColon = ownStartTokens.subList(colonIndex + 1,
				ownStartTokens.size());
		List<List<IToken>> statementTails = TokenStreamUtils.split(tokensAfterColon,
				ETokenType.COMMA);

		List<List<IToken>> statements = new ArrayList<List<IToken>>();
		for (List<IToken> tail : statementTails) {
			List<IToken> tokens = new ArrayList<IToken>(tail);
			tokens.addAll(0, tokensBeforeColon);
			statements.add(tokens);
		}
		return statements;
	}

}