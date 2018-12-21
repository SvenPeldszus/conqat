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

/**
 * This interface defines the methods which an architecture has to provide in
 * order to be used with the {@link ArchitectureAssessor}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50723 $
 * @ConQAT.Rating GREEN Hash: 73809C5DCA1E347361FE13974102DA3D
 */
public interface IArchitecture {

	/**
	 * Retrieves the list of all policies that exist in this architecture.
	 * Implementations of this interface may return more specific policies and
	 * also define this at the interface.
	 */
	Set<? extends IPolicy> getAllPolicies();

	/**
	 * Retrieves the list of all components that exist in this architecture.
	 * Implementations of this interface may return more specific components and
	 * also define this at the interface.
	 */
	Set<? extends IComponent> getAllComponents();

	/**
	 * Returns the pattern used to include only certain elements for the
	 * analysis of this architecture. The empty string indicates to include all
	 * elements.
	 */
	String getScopeIncludePattern();

	/**
	 * Returns the pattern used to exclude certain elements from the analysis
	 * for this architecture. The empty string indicates to exclude no elements.
	 */
	String getScopeExcludePattern();

	/**
	 * Returns whether this architecture is file-based (as opposed to
	 * type-based)
	 */
	boolean isFileBased();

	/**
	 * Retrieves the component with the given name or <code>null</code> if there
	 * is no component with the given name.
	 */
	IComponent getComponentByName(String name);
}
