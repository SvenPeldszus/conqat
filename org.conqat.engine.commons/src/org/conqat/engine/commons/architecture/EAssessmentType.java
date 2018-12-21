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
package org.conqat.engine.commons.architecture;

/**
 * Assessment result for edges in an architecture graph.
 * 
 * @author Elmar Juergens
 * @author $Author: heinemann $
 * @version $Rev: 51068 $
 * @ConQAT.Rating GREEN Hash: 804563901E5803F2515D53B4B70D304B
 */
public enum EAssessmentType {

	/** Dependencies underlying this edge are valid w.r.t. policies */
	VALID,

	/** Dependencies underlying this edge are invalid w.r.t. policies */
	INVALID,

	/** No dependencies underly this edge. It is unnecessary. */
	UNNECESSARY,
}