/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: MethodRule.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs.rules;

import static org.conqat.lib.scanner.ETokenType.BASE;
import static org.conqat.lib.scanner.ETokenType.COLON;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.THIS;
import static org.conqat.lib.scanner.ETokenType.WHERE;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.Context;
import org.conqat.engine.sourcecode.dataflow.heuristics.ControlFlowCreator;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.MethodRuleBase;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Transforms a method.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51545 $
 * @ConQAT.Rating YELLOW Hash: FFBB148927115032150C7B50BFD01991
 */
public class MethodRule extends MethodRuleBase {

	/**
	 * Matches invocations of the "base" or "this" constructor. Group 0 contains
	 * the matched tokens.
	 */
	private static final TokenPattern CONSTRUCTOR_INVOCATION_PATTERN = new TokenPattern()
			.sequence(ETokenType.RPAREN, ETokenType.COLON,
					EnumSet.of(ETokenType.BASE, ETokenType.THIS),
					ETokenType.LPAREN).group(0);

	/**
	 * {@inheritDoc}
	 * 
	 * Adds a node for any constructor invocation.
	 */
	@Override
	public Result transform(List<ShallowEntity> entities, Context context,
			ControlFlowCreator creator) {
		Result result = super.transform(entities, context, creator);

		List<IToken> methodTokens = entities.get(0).ownStartTokens();
		TokenPatternMatch constructorMatch = new TokenPattern().sequence(COLON)
				.alternative(BASE, THIS).group(0).sequence(LPAREN)
				.matchFirst(methodTokens);
		if (constructorMatch != null) {
			int constructorCallStartIndex = constructorMatch.groupIndices(0)
					.get(0);
			ETokenType[] types = { LBRACE };
			int constructorCallEndIndex = TokenStreamUtils.findLast(
					methodTokens, types);
			List<IToken> constructorCallTokens = methodTokens.subList(
					constructorCallStartIndex, constructorCallEndIndex);
			ControlFlowNode constructorNode = context.createNode(
					constructorCallTokens, false);
			ControlFlowNode.weaveAfter(result.getEntryNode(), constructorNode);
		}

		return result;
	}

	/** {@inheritDoc} */
	@Override
	protected List<IToken> extractParameterListTokens(ShallowEntity methodEntity) {
		List<IToken> methodTokens = methodEntity.ownStartTokens();
		ETokenType[] types = { LPAREN };
		int parameterStartIndex = TokenStreamUtils.find(methodTokens, types) + 1;

		int parameterEndIndex;

		TokenPatternMatch constructorMatch = CONSTRUCTOR_INVOCATION_PATTERN
				.matchFirst(methodTokens);
		if (constructorMatch != null) {
			IToken firstToken = constructorMatch.groupTokens(0).get(0);
			parameterEndIndex = methodTokens.indexOf(firstToken);
		} else {
			ETokenType[] types1 = { RPAREN };
			parameterEndIndex = TokenStreamUtils.findLast(methodTokens, types1);
		}

		TokenPatternMatch match = new TokenPattern().sequence(RPAREN, WHERE)
				.group(0).matchFirst(methodTokens);
		if (match != null) {
			parameterEndIndex = match.groupIndices(0).get(0);
		}

		List<IToken> parameterTokens = CollectionUtils.emptyList();
		if (parameterStartIndex != -1 && parameterEndIndex != -1) {
			parameterTokens = methodTokens.subList(parameterStartIndex,
					parameterEndIndex);
		}
		return parameterTokens;
	}

}