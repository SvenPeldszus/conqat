/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: MethodRule.java 51545 2015-01-19 09:35:28Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java.rules;

import java.util.List;

import org.conqat.engine.sourcecode.dataflow.heuristics.rules.MethodRuleBase;
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
 * @ConQAT.Rating YELLOW Hash: 0E4F4772E0BBA65734CC1FDA967DB4BE
 */
public class MethodRule extends MethodRuleBase {

	/** {@inheritDoc} */
	@Override
	protected List<IToken> extractParameterListTokens(ShallowEntity methodEntity) {
		List<IToken> methodTokens = methodEntity.ownStartTokens();
		ETokenType[] types = { ETokenType.LPAREN };
		int parameterStartIndex = TokenStreamUtils.find(methodTokens, types) + 1;
		ETokenType[] types1 = { ETokenType.RPAREN };
		int parameterEndIndex = TokenStreamUtils.findLast(methodTokens, types1);
		List<IToken> parameterTokens = CollectionUtils.emptyList();
		if (parameterStartIndex != -1 && parameterEndIndex != -1) {
			parameterTokens = methodTokens.subList(parameterStartIndex,
					parameterEndIndex);
		}
		return parameterTokens;
	}

}