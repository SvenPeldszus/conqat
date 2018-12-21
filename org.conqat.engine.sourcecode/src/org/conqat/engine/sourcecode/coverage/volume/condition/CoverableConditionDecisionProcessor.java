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

import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.coverage.volume.LineHint;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51252 $
 * @ConQAT.Rating GREEN Hash: 4725EA7444AA6A7FDF879C30A61CBFFF
 */
@AConQATProcessor(description = "Reports about coverable conditions/decisions.")
public class CoverableConditionDecisionProcessor extends
		CoverableConditionDecisionProcessorBase {

	/** {@inheritDoc} */
	@Override
	protected int processConditionDecision(Condition decision,
			List<Condition> subConditions, List<LineHint> hints) {
		addTrueFalseHints(decision, hints);

		if (subConditions.size() > 1) {
			for (Condition subCondition : subConditions) {
				addTrueFalseHints(subCondition, hints);
			}
		}
		return 0;
	}
}
