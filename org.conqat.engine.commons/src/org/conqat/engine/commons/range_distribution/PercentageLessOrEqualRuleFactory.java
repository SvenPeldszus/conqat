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
package org.conqat.engine.commons.range_distribution;

import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.lib.commons.assessment.ETrafficLightColor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 41751 $
 * @ConQAT.Rating GREEN Hash: C0943F980A5F0CCE8AA57A7696B093BF
 */
@AConQATProcessor(description = PercentageLessOrEqualRule.DOC)
public class PercentageLessOrEqualRuleFactory extends ConQATProcessorBase {

	/** The range names. */
	private final Set<String> rangeNames = new HashSet<String>();

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "threshold", attribute = "value", description = "The percentage threshold (inclusive)")
	public double threshold;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "secondary-metric", attribute = ConQATParamDoc.READKEY_KEY_NAME, description = "Key that defines the secondary "
			+ "metric this rule applies to.")
	public String secondaryMetric;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "colored-range", description = "Range this rule applies to.", minOccurrences = 1)
	public void addRange(
			@AConQATAttribute(name = "color", description = "Color that identifies the range.") ETrafficLightColor color) {
		rangeNames.add(color.name());
	}

	/** {@inheritDoc} */
	@Override
	public PercentageLessOrEqualRule process() {
		return new PercentageLessOrEqualRule(secondaryMetric, threshold,
				rangeNames);
	}
}
