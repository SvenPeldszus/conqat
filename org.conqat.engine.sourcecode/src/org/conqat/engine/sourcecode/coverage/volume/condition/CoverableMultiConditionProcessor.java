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
 * @ConQAT.Rating GREEN Hash: F039A686B24DB1574D37AD0E8E250468
 */
@AConQATProcessor(description = "Reports about coverable multi-conditions.")
public class CoverableMultiConditionProcessor extends
		CoverableConditionDecisionProcessorBase {

	/**
	 * Maximal number of subconditions for which we calculate multi-condition
	 * coverage. This is used to avoid computationally expensive calculations
	 * (exponential growth).
	 */
	public static final int SUBCONDITION_LIMIT = 12;

	/** {@inheritDoc} */
	@Override
	protected int processConditionDecision(Condition decision,
			List<Condition> subConditions, List<LineHint> hints) {
		if (subConditions.size() > SUBCONDITION_LIMIT) {
			hints.add(new LineHint(
					"Expression too complex for full multi-condition coverage: "
							+ decision + ". Has " + (1 << subConditions.size())
							+ " possible configurations.", decision
							.getLineNumber()));
			// return -1 as we add one hint
			return (1 << subConditions.size()) - 1;
		}

		hints.addAll(new MultiCondition(subConditions).createHintList());
		return 0;
	}

	/** {@inheritDoc} */
	@Override
	protected boolean includeBranchDecisions() {
		return false;
	}
}
