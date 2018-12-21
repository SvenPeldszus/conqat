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
package org.conqat.engine.architecture.assessment.shared;

import java.util.Set;

import org.conqat.engine.commons.architecture.EAssessmentType;

/**
 * A dependency between two components of an architecture. Apart from the
 * connection itself, the dependency is aware of the corresponding policy and
 * assessment status.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51068 $
 * @ConQAT.Rating GREEN Hash: D08836E38C05773E482488FD9D63A0CB
 */
public interface IDependency extends IPolicy {

	/** Retrieve the type of assessment between the source and target. */
	EAssessmentType getAssessment();

	/**
	 * Retrieves the dependencies between individual types which belong to this
	 * component-level dependency.
	 */
	Set<TypeDependency> getTypeDependencies();
}
