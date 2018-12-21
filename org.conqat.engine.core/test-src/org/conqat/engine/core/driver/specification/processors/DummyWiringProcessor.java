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

/**
 * Processor used for wiring up (does nothing).
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: 589F51017FECE9CAAE069E82844420B6
 */
@SuppressWarnings("unused")
@AConQATProcessor(description = "desc")
public class DummyWiringProcessor extends TestProcessorBase {

	/** Test method. */
	@AConQATParameter(description = "x", name = "x")
	public void setX(@AConQATAttribute(description = "x", name = "x") Object x) {
		// nothing to do here
	}

	/** Test method. */
	@AConQATParameter(description = "y", name = "y")
	public void setY(@AConQATAttribute(description = "y", name = "y") Object x) {
		// nothing to do here
	}

}