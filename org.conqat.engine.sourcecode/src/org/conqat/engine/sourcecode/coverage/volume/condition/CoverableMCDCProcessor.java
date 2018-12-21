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
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.coverage.volume.LineHint;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.collections.IdentityHashSet;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51252 $
 * @ConQAT.Rating GREEN Hash: CD6F3323D44C6BA686A4FAAA7A34FC3C
 */
@AConQATProcessor(description = "Reports about coverable MC/DC.")
public class CoverableMCDCProcessor extends
		CoverableConditionDecisionProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "An estimate on the number of tests required for MC/DC coverage of a condition.", type = "java.lang.Integer")
	public static final String MCDC_TEST_COUNT_KEY = "mc-dc-test-count";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Hints on the tests required for MC/DC coverage of a condition.", type = "java.util.List<org.conqat.engine.sourcecode.coverage.volume.LineHint>")
	public static final String MCDC_TEST_HINTS_KEY = "mc-dc-test-hints";

	/** MC/DC test hints to be filled during the analysis. */
	private List<LineHint> testHints;

	/**
	 * The number of additional test hint that are not reported in
	 * {@link #testHints}.
	 */
	private int additionalTestHintCount = 0;

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITokenResource root) {
		super.setUp(root);
		NodeUtils.addToDisplayList(root, MCDC_TEST_COUNT_KEY,
				MCDC_TEST_HINTS_KEY);
	}

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		testHints = new ArrayList<>();
		additionalTestHintCount = 0;

		super.processElement(element);

		element.setValue(MCDC_TEST_COUNT_KEY, testHints.size()
				+ additionalTestHintCount);
		element.setValue(MCDC_TEST_HINTS_KEY, testHints);
	}

	/** {@inheritDoc} */
	@Override
	protected int processConditionDecision(Condition decision,
			List<Condition> subConditions, List<LineHint> hints) {
		addTrueFalseHints(decision, hints);

		if (subConditions.size() > 1) {
			for (Condition subCondition : subConditions) {
				addTrueFalseHints(subCondition, hints);
				hints.add(subCondition
						.toLineHint("show that outcome affects decision"));
			}
		}

		try {
			addTestHints(decision, subConditions);
		} catch (ExpressionParsingException e) {
			getLogger().error(
					"Failed to process decision " + decision + ": "
							+ e.getMessage(), e);
		}

		return 0;
	}

	/** Adds test hints for a decision and its subconditions. */
	private void addTestHints(Condition decision, List<Condition> subConditions)
			throws ExpressionParsingException {
		if (subConditions.size() > CoverableMultiConditionProcessor.SUBCONDITION_LIMIT) {
			testHints.add(new LineHint(
					"Expression too complex for MC/DC hints at line "
							+ decision.getLineNumber()
							+ ". Number of required test cases is between "
							+ (subConditions.size() + 1) + " and " + 2
							* subConditions.size() + ".", decision
							.getLineNumber()));
			// -1 as we added one test hint already
			additionalTestHintCount += 2 * subConditions.size() - 1;
			return;
		}

		List<Configuration> configurations = new MultiCondition(subConditions)
				.getConfigurations();
		Set<Configuration> result = new IdentityHashSet<>();
		Map<Configuration, Boolean> evalCache = new IdentityHashMap<>();

		for (int i = 0; i < subConditions.size(); ++i) {
			if (!findMatchingConfigurations(decision, subConditions,
					configurations, result, evalCache, i)) {
				testHints.add(new LineHint(
						"MC/DC coverage not possible at line "
								+ decision.getLineNumber(), decision
								.getLineNumber()));
				return;
			}
		}

		testHints.addAll(MultiCondition.toHintList(subConditions,
				new ArrayList<>(result)));
	}

	/**
	 * Finds a pair of configurations that match (differ only in the position
	 * denoted by index) and evaluate to different boolean values. Returns
	 * whether such a pair has been found.
	 * 
	 * @param decision
	 *            the decision for the configurations.
	 * @param subConditions
	 *            the subConditions of the configurations.
	 * @param result
	 *            the matching configurations are added to this set if found.
	 * @param evalCache
	 *            cache used to store evaluation results that have already been
	 *            seen.
	 */
	private boolean findMatchingConfigurations(Condition decision,
			List<Condition> subConditions, List<Configuration> configurations,
			Set<Configuration> result, Map<Configuration, Boolean> evalCache,
			int index) throws ExpressionParsingException {
		Map<String, Configuration> matchingConfigurations = new HashMap<>();
		for (Configuration configuration : configurations) {
			String identifier = configuration.toMaskedString(index);
			if (!matchingConfigurations.containsKey(identifier)) {
				matchingConfigurations.put(identifier, configuration);
				continue;
			}

			Configuration other = matchingConfigurations.get(identifier);
			if (evaluateConfiguration(configuration, decision, subConditions,
					evalCache) != evaluateConfiguration(other, decision,
					subConditions, evalCache)) {
				result.add(configuration);
				result.add(other);
				return true;
			}
		}
		return false;
	}

	/** Returns the evaluation result of the given configuration. */
	private boolean evaluateConfiguration(Configuration configuration,
			Condition decision, List<Condition> subConditions,
			Map<Configuration, Boolean> evalCache)
			throws ExpressionParsingException {
		if (evalCache.containsKey(configuration)) {
			return evalCache.get(configuration);
		}

		boolean result = tokenTypeToBoolean(evaluateTokenExpression(buildExpression(
				configuration, decision, subConditions)));
		evalCache.put(configuration, result);
		return result;
	}

	/**
	 * Builds the boolean expression (as token types) to be evaluated for a
	 * specific configuration. This replaces all sub conditions with either true
	 * or false and thus should not contain any variables anymore.
	 */
	private List<ETokenType> buildExpression(Configuration configuration,
			Condition decision, List<Condition> subConditions) {
		List<ETokenType> expression = new ArrayList<>();
		int subConditionIndex = 0;
		IToken nextSubConditionToken = subConditions.get(subConditionIndex)
				.getTokens().get(0);
		int skipTokens = 0;
		for (IToken token : decision.getTokens()) {
			if (skipTokens > 0) {
				skipTokens -= 1;
				continue;
			}

			if (token == nextSubConditionToken) {
				if (configuration.get(subConditionIndex)) {
					expression.add(ETokenType.TRUE);
				} else {
					expression.add(ETokenType.FALSE);
				}

				skipTokens = subConditions.get(subConditionIndex).getTokens()
						.size() - 1;
				subConditionIndex += 1;
				if (subConditionIndex < subConditions.size()) {
					nextSubConditionToken = subConditions
							.get(subConditionIndex).getTokens().get(0);
				} else {
					nextSubConditionToken = null;
				}
			} else {
				expression.add(token.getType());
			}
		}
		return expression;
	}

	/**
	 * Evaluates the given Boolean expression and returns either
	 * {@link ETokenType#TRUE} or {@link ETokenType#FALSE}.
	 */
	private ETokenType evaluateTokenExpression(List<ETokenType> expression)
			throws ExpressionParsingException {
		List<ETokenType> reducedExpression = new ArrayList<>();
		for (int i = 0; i < expression.size(); ++i) {
			if (expression.get(i) == ETokenType.LPAREN) {
				int closingIndex = findClosingParenthesis(expression, i + 1);
				reducedExpression.add(evaluateTokenExpression(expression
						.subList(i + 1, closingIndex)));
				i = closingIndex;
			} else {
				reducedExpression.add(expression.get(i));
			}
		}

		return evaluateTokenExpressionWithoutParentheses(reducedExpression);
	}

	/**
	 * Evaluates a Boolean expression without parentheses and returns either
	 * {@link ETokenType#TRUE} or {@link ETokenType#FALSE}.
	 */
	private ETokenType evaluateTokenExpressionWithoutParentheses(
			List<ETokenType> expression) throws ExpressionParsingException {
		List<ETokenType> reduced = new ArrayList<>();
		for (int i = 0; i < expression.size(); ++i) {
			if (expression.get(i) == ETokenType.NOT) {
				i += 1;
				if (i >= expression.size()) {
					throw new ExpressionParsingException(
							"Negation without operand!");
				}
				// just skip double negation
				if (expression.get(i) != ETokenType.NOT) {
					reduced.add(booleanToTokenType(!tokenTypeToBoolean(expression
							.get(i))));
				}
			} else {
				reduced.add(expression.get(i));
			}
		}

		return evaluateTokenExpressionWithoutParenthesesAndNegation(reduced);
	}

	/**
	 * Evaluates a Boolean expression without parentheses and negation and
	 * returns either {@link ETokenType#TRUE} or {@link ETokenType#FALSE}.
	 */
	private ETokenType evaluateTokenExpressionWithoutParenthesesAndNegation(
			List<ETokenType> expression) throws ExpressionParsingException {
		expression = reduceExpressionWithBinaryOperator(expression,
				new IBinaryBooleanTokenOperation() {
					@Override
					public boolean isOperator(ETokenType type) {
						return type == ETokenType.ANDAND
								|| type == ETokenType.AND;
					}

					@Override
					public boolean evealuate(boolean a, boolean b) {
						return a && b;
					}
				});
		expression = reduceExpressionWithBinaryOperator(expression,
				new IBinaryBooleanTokenOperation() {
					@Override
					public boolean isOperator(ETokenType type) {
						return type == ETokenType.OROR || type == ETokenType.OR;
					}

					@Override
					public boolean evealuate(boolean a, boolean b) {
						return a || b;
					}
				});
		expression = reduceExpressionWithBinaryOperator(expression,
				new IBinaryBooleanTokenOperation() {
					@Override
					public boolean isOperator(ETokenType type) {
						return type == ETokenType.XOR;
					}

					@Override
					public boolean evealuate(boolean a, boolean b) {
						return a ^ b;
					}
				});

		if (expression.size() > 1) {
			throw new ExpressionParsingException(
					"Could not fully reduce expression. Remaining: "
							+ StringUtils.concat(expression, ", "));
		}

		return expression.get(0);
	}

	/** Returns the expression after reducing it with the given binary operator. */
	private List<ETokenType> reduceExpressionWithBinaryOperator(
			List<ETokenType> expression, IBinaryBooleanTokenOperation operation)
			throws ExpressionParsingException {
		List<ETokenType> reducedExpression = new ArrayList<>();
		for (int i = 0; i < expression.size(); ++i) {
			if (i == 0 || i == expression.size() - 1
					|| !operation.isOperator(expression.get(i))) {
				reducedExpression.add(expression.get(i));
				continue;
			}

			boolean left = tokenTypeToBoolean(reducedExpression
					.remove(reducedExpression.size() - 1));
			i += 1;
			boolean right = tokenTypeToBoolean(expression.get(i));
			reducedExpression.add(booleanToTokenType(operation.evealuate(left,
					right)));
		}
		return reducedExpression;
	}

	/** Converts a token type to a boolean value. */
	private boolean tokenTypeToBoolean(ETokenType type)
			throws ExpressionParsingException {
		switch (type) {
		case TRUE:
			return true;
		case FALSE:
			return false;
		default:
			throw new ExpressionParsingException(
					"Expected boolean literal but had " + type);
		}
	}

	/** Converts a boolean value to matching token type. */
	private ETokenType booleanToTokenType(boolean value) {
		if (value) {
			return ETokenType.TRUE;
		}
		return ETokenType.FALSE;
	}

	/** Returns the index of the closing parenthesis (dealing with nesting). */
	private int findClosingParenthesis(List<ETokenType> expression,
			int startIndex) throws ExpressionParsingException {
		int nesting = 0;
		for (int i = startIndex; i < expression.size(); ++i) {
			switch (expression.get(i)) {
			case LPAREN:
				nesting += 1;
				break;
			case RPAREN:
				if (nesting == 0) {
					return i;
				}
				nesting -= 1;
				break;
			}
		}
		throw new ExpressionParsingException("Missing closing parenthesis!");
	}

	/** Interface for boolean operators. */
	private abstract static interface IBinaryBooleanTokenOperation {

		/** Returns whether the given token type corresponds to the operator. */
		boolean isOperator(ETokenType type);

		/** Evaluates the operator on two parameters. */
		boolean evealuate(boolean a, boolean b);
	}

	/** Exception used to signal local parsing problems. */
	private static class ExpressionParsingException extends Exception {

		/** Serial version UID. */
		private static final long serialVersionUID = 1L;

		/** Constructor. */
		public ExpressionParsingException(String message) {
			super(message);
		}

	}
}
