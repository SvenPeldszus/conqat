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
package org.conqat.engine.code_clones.normalization.repetition;

import org.conqat.engine.code_clones.core.Unit;
import org.conqat.engine.code_clones.normalization.provider.IUnitProvider;
import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.text.ITextResource;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 50691 $
 * @ConQAT.Rating GREEN Hash: CBDE11852B7F9FF5B2082841015DF97A
 */
@AConQATProcessor(description = "A normalization stage that replaces repetitive sequences of units with a sentinel.")
public class RepetitiveUnitFilterFactory extends ConQATProcessorBase {

	/** Default value for {@link #maxRepetitionPatternSize}. */
	private static final int DEFAULT_MAX_REPETITION_PATTERN_SIZE = 2;

	/** Default value for {@link #minRepetitionRegionSize}. */
	private static final int DEFAULT_MIN_REPETITION_REGION_SIZE = 20;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "repetition-pattern-size", attribute = "max", optional = true, description = ""
			+ "The maximal size of repetition pattern checked. This should not be too large to prevent high running times. Default value is "
			+ DEFAULT_MAX_REPETITION_PATTERN_SIZE + ".")
	public int maxRepetitionPatternSize = DEFAULT_MAX_REPETITION_PATTERN_SIZE;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "repetition-region-size", attribute = "min", optional = true, description = ""
			+ "The minimal size of a repetition region to be excluded. This should not be too large to prevent high running times. Default value is "
			+ DEFAULT_MIN_REPETITION_REGION_SIZE + ".")
	public int minRepetitionRegionSize = DEFAULT_MIN_REPETITION_REGION_SIZE;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "unit", attribute = "provider", description = "The normalization providing the tokens.")
	public IUnitProvider<ITextResource, Unit> unitProvider;

	/** {@inheritDoc} */
	@Override
	public IUnitProvider<ITextResource, Unit> process() {
		return new RepetitiveUnitFilter(unitProvider, maxRepetitionPatternSize,
				minRepetitionRegionSize);
	}
}
