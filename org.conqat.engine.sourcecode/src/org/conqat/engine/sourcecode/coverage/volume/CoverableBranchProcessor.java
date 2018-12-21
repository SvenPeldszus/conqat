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
package org.conqat.engine.sourcecode.coverage.volume;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.coverage.volume.condition.Condition;
import org.conqat.engine.sourcecode.coverage.volume.condition.ConditionParserFactory;
import org.conqat.engine.sourcecode.coverage.volume.condition.IConditionExtractor;
import org.conqat.engine.sourcecode.coverage.volume.condition.ISubConditionParser;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * A processor that counts and list coverable branches.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51020 $
 * @ConQAT.Rating GREEN Hash: 4898B2E06B9C897B76FB14FE3D868FD1
 */
@AConQATProcessor(description = "Reports about coverable branches.")
public class CoverableBranchProcessor extends CoverableVolumeProcessorBase {

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		List<ShallowEntity> entities = ShallowParserFactory.parse(element,
				getLogger());

		if (!ConditionalStatementSubtypes.supportsLanguage(element
				.getLanguage())) {
			throw new ConQATException("Language "
					+ element.getLanguage().getReadableName()
					+ " not supported!");
		}

		IConditionExtractor conditionExtractor = ConditionParserFactory
				.createConditionExtractor(element.getLanguage());
		ISubConditionParser subConditionParser = ConditionParserFactory
				.createSubConditionParser(element.getLanguage());

		List<CodeBranch> branches = new ArrayList<>();
		List<ShallowEntity> methods = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.METHOD);
		for (ShallowEntity method : methods) {
			List<ShallowEntity> statements = ShallowEntityTraversalUtils
					.listEntitiesOfType(method.getChildren(),
							EShallowEntityType.STATEMENT);
			for (ShallowEntity entity : statements) {
				addBranchesFromEntity(entity, branches, conditionExtractor,
						subConditionParser);
			}
		}

		element.setValue(COVERABLE_VOLUME_KEY, branches.size());
		element.setValue(COVERABLE_HINTS_KEY, toLineHints(branches));
	}

	/** Converts branches to line hints. */
	private List<LineHint> toLineHints(List<CodeBranch> branches) {
		List<LineHint> hints = new ArrayList<>();
		for (CodeBranch branch : branches) {
			hints.add(new LineHint(branch.toString(), branch.getLine()));
		}
		return hints;
	}

	/** Adds all branches for the given statement to the list of branches. */
	private void addBranchesFromEntity(ShallowEntity entity,
			List<CodeBranch> branches, IConditionExtractor conditionExtractor,
			ISubConditionParser subConditionParser) throws ConQATException {
		if (entity.isEmpty()) {
			return;
		}

		ELanguage language = CollectionUtils.getAny(entity.includedTokens())
				.getLanguage();
		if ((ConditionalStatementSubtypes.isConditionalStatement(entity) && hasNonConstantSubCondition(
				entity, conditionExtractor, subConditionParser))
				|| (language == ELanguage.ADA && isExitWhen(entity))) {
			branches.add(new CodeBranch(entity, true));

			// for Ada's elsif we do not want to include the false case
			if (!SubTypeNames.ELSIF.equals(entity.getSubtype())) {
				branches.add(new CodeBranch(entity, false));
			}
		} else if (language == ELanguage.ADA
				&& SubTypeNames.CASE.equalsIgnoreCase(entity.getSubtype())) {
			handleAdaCase(entity, branches);
		} else if ((language == ELanguage.CPP || language == ELanguage.CS || language == ELanguage.JAVA)
				&& SubTypeNames.SWITCH.equals(entity.getSubtype())) {
			handleClikeSwitch(entity, branches);
		}
	}

	/**
	 * Returns true if the method has a non-constant sub-condition. Also returns
	 * true for entities without "real" condition, such as counted for loops in
	 * Ada.
	 */
	private boolean hasNonConstantSubCondition(ShallowEntity entity,
			IConditionExtractor conditionExtractor,
			ISubConditionParser subConditionParser) throws ConQATException {
		Condition condition = conditionExtractor.extractCondition(entity);
		if (condition == null) {
			return true;
		}

		List<Condition> subConditions = subConditionParser
				.getSubConditions(condition);
		return !subConditions.isEmpty();
	}

	/** Returns whether the entity is an "exit when" statement. */
	public static boolean isExitWhen(ShallowEntity entity) {
		return TokenStreamUtils.startsWith(entity.includedTokens(),
				ETokenType.EXIT, ETokenType.WHEN);
	}

	/**
	 * Handles the switch statement from C/C++, C# and Java, and adds branches
	 * to the given list.
	 */
	private void handleClikeSwitch(ShallowEntity entity,
			List<CodeBranch> branches) {
		boolean hadDefault = false;
		boolean previousWasCase = false;

		for (ShallowEntity child : entity.getChildren()) {
			if (child.getType() != EShallowEntityType.META) {
				previousWasCase = false;
				continue;
			}

			switch (child.getSubtype()) {
			case SubTypeNames.CASE:
				if (!previousWasCase) {
					branches.add(new CodeBranch(child));
				}
				previousWasCase = true;
				break;
			case SubTypeNames.DEFAULT:
				branches.add(new CodeBranch(child));
				hadDefault = true;
				previousWasCase = false;
				break;
			default:
				previousWasCase = false;
			}
		}

		if (!hadDefault) {
			branches.add(new CodeBranch(entity, SubTypeNames.DEFAULT));
		}
	}

	/** Handles the Ada case statement and adds branches to the given list. */
	private void handleAdaCase(ShallowEntity entity, List<CodeBranch> branches) {
		boolean hadWhenOthers = false;
		for (ShallowEntity child : entity.getChildren()) {
			if (child.getType() == EShallowEntityType.STATEMENT
					&& SubTypeNames.WHEN.equalsIgnoreCase(child.getSubtype())) {
				branches.add(new CodeBranch(child));
				List<IToken> includedTokens = child.includedTokens();
				if (includedTokens.size() > 1
						&& includedTokens.get(1).getType() == ETokenType.OTHERS) {
					hadWhenOthers = true;
				}
			}
		}
		if (!hadWhenOthers) {
			branches.add(new CodeBranch(entity, "others"));
		}
	}
}
