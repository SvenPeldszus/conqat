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
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.coverage.volume.ConditionalStatementSubtypes;
import org.conqat.engine.sourcecode.coverage.volume.CoverableBranchProcessor;
import org.conqat.engine.sourcecode.coverage.volume.CoverableVolumeProcessorBase;
import org.conqat.engine.sourcecode.coverage.volume.LineHint;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for processors that work on conditions and decisions.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51252 $
 * @ConQAT.Rating GREEN Hash: FBCE60E0D25C6B0997936C8ECD61B5D6
 */
public abstract class CoverableConditionDecisionProcessorBase extends
		CoverableVolumeProcessorBase {

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		ELanguage language = element.getLanguage();
		if (!ConditionalStatementSubtypes.supportsLanguage(language)) {
			throw new ConQATException("Unsupported language: "
					+ language.getReadableName() + "!");
		}

		IConditionExtractor conditionExtractor = ConditionParserFactory
				.createConditionExtractor(language);
		ISubConditionParser subConditionParser = ConditionParserFactory
				.createSubConditionParser(language);

		List<ShallowEntity> entities = ShallowParserFactory.parse(element,
				getLogger());

		List<LineHint> hints = new ArrayList<>();
		int additionalVolumeCount = 0;
		for (ShallowEntity entity : ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.STATEMENT)) {
			additionalVolumeCount += processStatementEntity(entity, language,
					conditionExtractor, subConditionParser, hints);
		}

		element.setValue(COVERABLE_VOLUME_KEY,
				additionalVolumeCount + hints.size());
		element.setValue(COVERABLE_HINTS_KEY, hints);
	}

	/**
	 * Processes the given statement entity and places {@link LineHint}s into
	 * the hint list. Returns the amount of additional volume that is not
	 * reported as explicit {@link LineHint}s.
	 */
	private int processStatementEntity(ShallowEntity entity,
			ELanguage language, IConditionExtractor conditionExtractor,
			ISubConditionParser subConditionParser, List<LineHint> hints)
			throws ConQATException {
		if (entity.isEmpty()) {
			return 0;
		}

		if (ConditionalStatementSubtypes.isConditionalStatement(entity)
				|| (language == ELanguage.ADA && CoverableBranchProcessor
						.isExitWhen(entity))) {
			Condition condition = conditionExtractor.extractCondition(entity);
			if (condition == null) {
				handleEntityWithoutCondition(hints, entity);
			} else {
				return processDecisionIfHasSubconditions(condition,
						subConditionParser, hints);
			}
		} else if (language == ELanguage.ADA
				&& SubTypeNames.CASE.equalsIgnoreCase(entity.getSubtype())) {
			handleAdaCase(entity, hints);
		} else if ((language == ELanguage.CPP || language == ELanguage.CS || language == ELanguage.JAVA)
				&& SubTypeNames.SWITCH.equals(entity.getSubtype())) {
			handleClikeSwitch(entity, hints);
		} else if (entity.getChildren().isEmpty()) {
			Condition condition = conditionExtractor
					.extractGeneralCondition(entity.includedTokens());
			if (condition != null) {
				return processDecisionIfHasSubconditions(condition,
						subConditionParser, hints);
			}
		}
		return 0;
	}

	/**
	 * Processes the given decision if it has subconditions. Returns the amount
	 * of additional volume that is not reported as explicit {@link LineHint}s.
	 */
	private int processDecisionIfHasSubconditions(Condition decision,
			ISubConditionParser subConditionParser, List<LineHint> hints) {
		List<Condition> subconditions = subConditionParser
				.getSubConditions(decision);
		if (!subconditions.isEmpty()) {
			return processConditionDecision(decision, subconditions, hints);
		}
		return 0;
	}

	/**
	 * These are usually loop constructs without explicit condition, such as
	 * for-each loops.
	 */
	private void handleEntityWithoutCondition(List<LineHint> hints,
			ShallowEntity entity) {
		if (!includeBranchDecisions()) {
			return;
		}

		hints.add(new LineHint(entity.toLocalString() + " - continue", entity
				.getStartLine()));
		hints.add(new LineHint(entity.toLocalString() + " - break", entity
				.getStartLine()));
	}

	/** Handles Ada's case statement. */
	private void handleAdaCase(ShallowEntity entity, List<LineHint> hints) {
		if (!includeBranchDecisions()) {
			return;
		}

		for (ShallowEntity child : entity.getChildren()) {
			if (child.getType() == EShallowEntityType.STATEMENT
					&& SubTypeNames.WHEN.equalsIgnoreCase(child.getSubtype())) {
				List<IToken> includedTokens = child.includedTokens();
				if (includedTokens.size() > 1
						&& includedTokens.get(1).getType() == ETokenType.OTHERS) {
					continue;
				}

				hints.add(new LineHint("case in line " + entity.getStartLine()
						+ ": when in line " + child.getStartLine(), entity
						.getStartLine()));
			}
		}

		hints.add(new LineHint("case in line " + entity.getStartLine()
				+ ": when other", entity.getStartLine()));
	}

	/** Handles the switch construct in C-like languages. */
	private void handleClikeSwitch(ShallowEntity entity, List<LineHint> hints) {
		if (!includeBranchDecisions()) {
			return;
		}

		boolean previousWasCase = false;

		for (ShallowEntity child : entity.getChildren()) {
			if (child.getType() != EShallowEntityType.META) {
				previousWasCase = false;
				continue;
			}

			switch (child.getSubtype()) {
			case SubTypeNames.CASE:
				if (!previousWasCase) {
					hints.add(new LineHint("switch in line "
							+ entity.getStartLine() + ": case in line "
							+ child.getStartLine(), entity.getStartLine()));
				}
				previousWasCase = true;
				break;
			default:
				previousWasCase = false;
			}
		}

		hints.add(new LineHint("switch in line " + entity.getStartLine()
				+ ": default case", entity.getStartLine()));
	}

	/**
	 * Processes a condition/decision. Returns the amount of additional volume
	 * that is not reported as explicit {@link LineHint}s.
	 * 
	 * @param decision
	 *            the decision (i.e. full condition).
	 * @param subconditions
	 *            the subconditions of this decision.
	 * @param hints
	 *            the list into which resulting hints should be placed.
	 */
	protected abstract int processConditionDecision(Condition decision,
			List<Condition> subconditions, List<LineHint> hints);

	/**
	 * Returns whether branch decisions (i.e. without boolean condition) should
	 * be included in the hints.
	 */
	protected boolean includeBranchDecisions() {
		return true;
	}

	/** Adds hints for both true and false evaluation of a condition. */
	protected void addTrueFalseHints(Condition condition, List<LineHint> hints) {
		hints.add(condition.toLineHint("true"));
		hints.add(condition.toLineHint("false"));
	}

}
