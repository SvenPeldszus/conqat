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

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.IToken;

/**
 * Interface for extracting conditions from shallow entities.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51020 $
 * @ConQAT.Rating GREEN Hash: 3DF5478276F64D6070FD564481E95745
 */
public interface IConditionExtractor {

	/** Returns the condition from the given shallow entity or null. */
	Condition extractCondition(ShallowEntity conditionalEntities)
			throws ConQATException;

	/**
	 * Attempts to find a condition in the given tokens and returns the largest
	 * possible condition or null.
	 */
	Condition extractGeneralCondition(List<IToken> tokens)
			throws ConQATException;
}
