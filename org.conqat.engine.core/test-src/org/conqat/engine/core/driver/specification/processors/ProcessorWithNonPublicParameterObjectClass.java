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

import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.IConQATParameterObject;

/**
 * Processor for testing purposes.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: 92F66F450823D35307D7AF4664D1E3FC
 */
@AConQATProcessor(description = "desc")
public class ProcessorWithNonPublicParameterObjectClass extends
		TestProcessorBase {

	/** test parameter object. */
	@AConQATParameterObject
	public Parameter param = new Parameter();

	/** Parameter object that is private. */
	private class Parameter implements IConQATParameterObject {
		// does not have any declaration.
	}

}