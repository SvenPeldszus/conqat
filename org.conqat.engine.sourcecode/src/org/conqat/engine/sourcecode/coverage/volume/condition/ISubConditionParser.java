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

/**
 * An interface for splitting conditions into sub-conditions for condition
 * coverage analysis.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50762 $
 * @ConQAT.Rating GREEN Hash: 7DC050067C55CAB8EADE92C16B79CFC6
 */
public interface ISubConditionParser {

	/** Splits a condition into a list of sub-conditions. */
	List<Condition> getSubConditions(Condition condition);
}
