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
package org.conqat.engine.sourcecode.coverage;

/**
 * Information on how a line was covered.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48583 $
 * @ConQAT.Rating GREEN Hash: B937C5C71F782F1081FC1B3FCC82E874
 */
public enum ELineCoverage {

	/** The line was not covered at all */
	NOT_COVERED,

	/** The line was partially covered */
	PARTIALLY_COVERED,

	/** The line was fully covered */
	FULLY_COVERED;

}
