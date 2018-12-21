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
package org.conqat.engine.persistence.store.branched;

/**
 * The current status of a branched commit.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51574 $
 * @ConQAT.Rating GREEN Hash: D37A9577FD2D378CE4E86492DDF1CC22
 */
public enum ECommitStatus {

	/** Indicates that this commit is in the process of being created. */
	CREATING,

	/** Indicates that this commit is created an can be written to. */
	WRITEABLE,

	/**
	 * Indicates that this commit is sealed and should no longer be written to.
	 */
	SEALED
}
