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
package org.conqat.engine.commons.config;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.logging.ELogLevel;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: goeb $
 * @version $Rev: 49155 $
 * @ConQAT.Rating GREEN Hash: 548728C33C526920C33F33EBE52416ED
 */
@AConQATProcessor(description = KeyedConfigValueBase.DESCRIPTION_PREFIX
		+ "org.conqat.engine.core.logging.ELogLevel"
		+ KeyedConfigValueBase.DESCRIPTION_SUFFIX)
public class KeyedConfigLogLevelValue extends KeyedConfigValueBase<ELogLevel> {

	/** {@inheritDoc} */
	@Override
	protected Class<ELogLevel> getValueClass() {
		return ELogLevel.class;
	}

}
