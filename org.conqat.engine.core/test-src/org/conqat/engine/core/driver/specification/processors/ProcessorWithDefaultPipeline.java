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
 * @ConQAT.Rating GREEN Hash: 2C988A5590A4D36A7FC4F3D832093C52
 */
@SuppressWarnings("unused")
@AConQATProcessor(description = "desc")
public class ProcessorWithDefaultPipeline extends TestProcessorBase {

	/** Test method. */
	@AConQATParameter(description = "mult_desc", name = "mult", minOccurrences = 1, maxOccurrences = 1)
	public void mult(
			@AConQATAttribute(name = "a", description = "a", defaultValue = "pipeline-break") @APipelineSource String a) {
		// nothing to do here
	}

	/** {@inheritDoc} */
	@Override
	public String process() {
		return null;
	}

}