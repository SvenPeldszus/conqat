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

import org.conqat.lib.commons.logging.ILogger;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.IConQATProcessor;
import org.conqat.engine.core.core.IConQATProcessorInfo;

/**
 * Processor for testing purposes.
 * 
 * The choice for ILogger is arbitrary here; we just need an interface (and sub
 * interface) which in itself is not generic.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 36897 $
 * @ConQAT.Rating GREEN Hash: ED844A4533E8DBE3FE19092C8751E0FF
 */
@SuppressWarnings("unused")
@AConQATProcessor(description = "invalid, because generic")
public abstract class AbstractGenericProcessorBase<E extends ILogger> implements
		IConQATProcessor {

	/** test field */
	@AConQATFieldParameter(parameter = "field", attribute = "attr", description = "")
	public E exposed;

	/** test method */
	@AConQATParameter(description = "param", name = "param")
	public void param(@AConQATAttribute(name = "a", description = "a") E e) {
		// nothing to do here
	}

	/** {@inheritDoc} */
	@Override
	public void init(IConQATProcessorInfo processorInfo) {
		// nothing to do here
	}

	/** {@inheritDoc} */
	@Override
	public E process() {
		return null;
	}

}