/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.coverage.volume.condition;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Condition parser that splits on certain tokens.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51131 $
 * @ConQAT.Rating GREEN Hash: 9B17A4720F4DD7A821063218E6D0FB14
 */
public class SplitSubConditionParser implements ISubConditionParser {

	/** The token types on which to split. */
	private final Set<ETokenType> splitTokenTypes;

	/**
	 * The token types on which to split only top-level, i.e. not in
	 * parentheses.
	 */
	private final Set<ETokenType> topLevelSplitTokenTypes;

	/** Constructor. */
	public SplitSubConditionParser(Set<ETokenType> splitTokenTypes) {
		this(splitTokenTypes, EnumSet.noneOf(ETokenType.class));
	}

	/** Constructor. */
	public SplitSubConditionParser(Set<ETokenType> splitTokenTypes,
			Set<ETokenType> topLevelSplitTokenTypes) {
		this.splitTokenTypes = splitTokenTypes;
		this.topLevelSplitTokenTypes = topLevelSplitTokenTypes;
	}

	/** {@inheritDoc} */
	@Override
	public List<Condition> getSubConditions(Condition condition) {
		List<Condition> subConditions = new ArrayList<>();
		Condition currentCondition = new Condition();
		int parenthesisNesting = 0;
		for (IToken token : condition.getTokens()) {
			ETokenType tokenType = token.getType();
			if (tokenType == ETokenType.LPAREN) {
				parenthesisNesting += 1;
			} else if (tokenType == ETokenType.RPAREN) {
				parenthesisNesting -= 1;
			}

			if (splitTokenTypes.contains(tokenType)
					|| (parenthesisNesting == 0 && topLevelSplitTokenTypes
							.contains(tokenType))) {
				addConditionIfNotConstant(currentCondition, subConditions);
				currentCondition = new Condition();
			} else {
				currentCondition.appendToken(token);
			}
		}

		// Add the last token if there is still no sub-condition or if it is not
		// empty.
		if (subConditions.isEmpty() || !currentCondition.isEmpty()) {
			addConditionIfNotConstant(currentCondition, subConditions);
		}

		return subConditions;
	}

	/**
	 * Adds the given condition unless it is a constant (which is checked by
	 * simple heuristics).
	 */
	private void addConditionIfNotConstant(Condition condition,
			List<Condition> subConditions) {
		condition.cleanupParentheses();
		if (condition.isEmpty()) {
			return;
		}

		if (condition.getTokens().size() == 1) {
			String tokenText = condition.getTokens().get(0).getText()
					.toLowerCase();
			if (tokenText.equals("true") || tokenText.equals("false")) {
				return;
			}
		}

		subConditions.add(condition);
	}
}
