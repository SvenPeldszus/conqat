/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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
package org.conqat.engine.core.driver.specification.processors;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.APipelineSource;

/**
 * Processor for testing purposes.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: B5972B7E3D9376CDE06F1F6EC756BD66
 */
@AConQATProcessor(description = "This processor has different types in the return value and the pipeline attribute!")
public class ProcessorWithIncompatiblePipeline extends TestProcessorBase {

	/** test method. */
	@SuppressWarnings("unused")
	@AConQATParameter(description = "xset_desc", name = "xset", minOccurrences = 1, maxOccurrences = 1)
	public void setX(
			@AConQATAttribute(description = "at_a_desc", name = "at_a") @APipelineSource int a,
			@AConQATAttribute(description = "at_b_desc", name = "at_b", defaultValue = "test") String b) {
		// nothing to do here
	}

	/** {@inheritDoc} */
	@Override
	public String process() {
		return null;
	}

}