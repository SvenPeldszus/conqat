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

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATProcessor;

/**
 * Processor for testing purposes.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: 268415E485802E495B7F1A5F7A2816F6
 */
@AConQATProcessor(description = "desc")
public class ProcessorWithNonPublicKey extends TestProcessorBase {

	/** test key that is no string. */
	@AConQATKey(type = "java.lang.String", description = "desc")
	private static final String key = null;

}