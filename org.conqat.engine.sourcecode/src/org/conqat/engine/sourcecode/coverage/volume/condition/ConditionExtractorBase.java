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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for condition extractors.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51131 $
 * @ConQAT.Rating GREEN Hash: EBF7ACC94DADCF630E36E420A3313096
 */
public abstract class ConditionExtractorBase implements IConditionExtractor {

	/** Set of operators that create a boolean result. */
	private final Set<ETokenType> booleanResultOperators;

	/** Constructor. */
	protected ConditionExtractorBase(Set<ETokenType> booleanResultOperators) {
		this.booleanResultOperators = booleanResultOperators;
	}

	/** {@inheritDoc} */
	@Override
	public Condition extractGeneralCondition(List<IToken> tokens)
			throws ConQATException {
		List<Condition> conditions = new ArrayList<>();

		extractGeneralConditions(eliminateTemplates(tokens), conditions);

		if (conditions.isEmpty()) {
			return null;
		}

		// returns the longest condition (in term of tokens).
		return Collections.max(conditions, new Comparator<Condition>() {
			@Override
			public int compare(Condition condition1, Condition condition2) {
				return Integer.compare(condition1.getTokens().size(),
						condition2.getTokens().size());
			}
		});
	}

	/**
	 * Returns a new token list with all template parameters removed. Template
	 * parameters are of the form "LT IDENTIFIER (COMMA, IDENTIFIER)* GT".
	 */
	private List<IToken> eliminateTemplates(List<IToken> tokens) {
		List<IToken> result = new ArrayList<>();

		// as soon as this is non-empty, we are in a possible template list
		List<IToken> templateBuffer = new ArrayList<>();

		for (IToken token : tokens) {
			ETokenType type = token.getType();
			if (templateBuffer.isEmpty()) {
				if (type == ETokenType.LT) {
					templateBuffer.add(token);
				} else {
					result.add(token);
				}
				continue;
			}

			if (canCloseTemplate(templateBuffer.size())
					&& type == ETokenType.GT) {
				// had a template -> eliminate
				templateBuffer.clear();
			} else if (templateContinuationType(templateBuffer.size()) == type) {
				templateBuffer.add(token);
			} else {
				// not template, so discard and continue normally
				result.addAll(templateBuffer);
				templateBuffer.clear();
				result.add(token);
			}
		}
		return result;
	}

	/**
	 * Returns, whether the next token could be a closing template token, given
	 * the number of previous (template candidate) tokens. This can happen only
	 * after even number of tokens and not at the very start.
	 */
	private boolean canCloseTemplate(int numberOfPreviousTokens) {
		return numberOfPreviousTokens > 1 && (numberOfPreviousTokens % 2) == 0;
	}

	/**
	 * Returns, which token type would be expected to continue template
	 * parameters, based on the number of previous (template candidate) tokens.
	 */
	private ETokenType templateContinuationType(int numberOfPreviousTokens) {
		if (numberOfPreviousTokens % 2 == 0) {
			return ETokenType.COMMA;
		}
		return ETokenType.IDENTIFIER;
	}

	/** Extracts all found conditions from the given tokens. */
	private void extractGeneralConditions(List<IToken> tokens,
			List<Condition> conditions) throws ConQATException {
		List<IToken> withoutMethods = new ArrayList<>();
		List<IToken> methodParameterBuffer = new ArrayList<>();

		// while this is < 0, we are outside of a method call
		int methodCallNesting = -1;

		ETokenType previousTokenType = null;
		for (IToken token : tokens) {
			ETokenType type = token.getType();
			if (methodCallNesting < 0) {
				if ((type == ETokenType.LPAREN || type == ETokenType.LBRACK)
						&& previousTokenType == ETokenType.IDENTIFIER) {
					methodCallNesting = 0;
					previousTokenType = null;
					withoutMethods.set(
							withoutMethods.size() - 1,
							extendMethodToken(
									CollectionUtils.getLast(withoutMethods),
									type));
					continue;
				}

				withoutMethods.add(token);
				previousTokenType = type;
				continue;
			}

			switch (type) {
			case LPAREN:
			case LBRACK:
				methodCallNesting += 1;
				break;
			case RPAREN:
			case RBRACK:
				if (methodCallNesting == 0) {
					extractGeneralConditions(methodParameterBuffer, conditions);
					methodParameterBuffer.clear();
				}
				methodCallNesting -= 1;
				break;
			case COMMA:
				if (methodCallNesting == 0) {
					extractGeneralConditions(methodParameterBuffer, conditions);
					methodParameterBuffer.clear();
				}
				break;
			default:
				methodParameterBuffer.add(token);
			}
		}

		if (methodCallNesting >= 0) {
			throw new ConQATException("Misbalanced parentheses!");
		}

		extractGeneralConditionWithoutCall(withoutMethods, conditions);
	}

	/**
	 * Extends a method call token with "(...)" or "[...]" depending on the
	 * token type of the parenthesis.
	 */
	private IToken extendMethodToken(IToken token, ETokenType parenthesisType) {
		String extension = "(...)";
		if (parenthesisType == ETokenType.LBRACK) {
			extension = "[...]";
		}
		return token.newToken(ETokenType.IDENTIFIER, token.getOffset(),
				token.getLineNumber(), token.getText() + extension,
				token.getOriginId());
	}

	/**
	 * Extracts a general condition (if found) and puts it into the conditions
	 * list. The tokens should not contain template parameters or method calls.
	 */
	protected void extractGeneralConditionWithoutCall(List<IToken> tokens,
			List<Condition> conditions) {
		// must use 1 here, as first element (index 0) is expected to be operand
		int firstMatch = 1;
		while (firstMatch < tokens.size()
				&& !isBooleanResultOperator(tokens, firstMatch)) {
			firstMatch += 1;
		}

		if (firstMatch >= tokens.size()) {
			return;
		}

		// must use -2 here, as last entry (-1) should be operand 
		int lastMatch = tokens.size() - 2;
		if (lastMatch < firstMatch) {
			return;
		}
		
		while (!isBooleanResultOperator(tokens, lastMatch)) {
			lastMatch -= 1;
		}

		firstMatch -= 1;
		lastMatch += 1;

		// balance parentheses left
		int minOpen = 0;
		int balance = 0;
		for (int i = firstMatch; i <= lastMatch; ++i) {
			balance = updateParenthesisBalance(balance, tokens.get(i));
			minOpen = Math.min(balance, minOpen);
		}
		while (minOpen < 0 && firstMatch > 0) {
			firstMatch -= 1;
			minOpen = updateParenthesisBalance(minOpen, tokens.get(firstMatch));
		}

		// balance parentheses right
		balance = 0;
		for (int i = firstMatch; i <= lastMatch; ++i) {
			balance = updateParenthesisBalance(balance, tokens.get(i));
		}
		while (balance > 0 && lastMatch < tokens.size() - 1) {
			lastMatch += 1;
			balance = updateParenthesisBalance(balance, tokens.get(lastMatch));
		}

		conditions
				.add(new Condition(tokens.subList(firstMatch, lastMatch + 1)));
	}

	/**
	 * Returns an updated parenthesis balance (i.e. number of open parentheses)
	 * based on the previous value and the given token.
	 */
	private int updateParenthesisBalance(int balance, IToken token) {
		switch (token.getType()) {
		case LPAREN:
			return balance + 1;
		case RPAREN:
			return balance - 1;
		default:
			return balance;
		}
	}

	/**
	 * Returns whether the token in the list at given index is an operator with
	 * boolean result.
	 */
	private boolean isBooleanResultOperator(List<IToken> tokens, int index) {
		return booleanResultOperators.contains(tokens.get(index).getType())
				&& !isPartOfDoubleOperator(index, tokens);
	}

	/**
	 * Returns whether the token at given index is part of a sequence of two
	 * consecutive operators.
	 */
	private boolean isPartOfDoubleOperator(int index, List<IToken> tokens) {
		boolean isDoubleOperatorBefore = index > 0
				&& isDoubleOperator(tokens.get(index - 1).getType(), tokens
						.get(index).getType());
		boolean isDoubleOperatorAfter = index < tokens.size() - 1
				&& isDoubleOperator(tokens.get(index).getType(),
						tokens.get(index + 1).getType());
		return isDoubleOperatorBefore || isDoubleOperatorAfter;
	}

	/**
	 * Returns whether the given pair of token types is a operator combination
	 * that has to be ignored for the language.
	 */
	protected abstract boolean isDoubleOperator(ETokenType type1,
			ETokenType type2);
}
