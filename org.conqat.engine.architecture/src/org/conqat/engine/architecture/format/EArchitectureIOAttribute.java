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
package org.conqat.engine.architecture.format;

/**
 * Enum of attributes used in the architecture definition file and architecture
 * assessment file.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 50698 $
 * @ConQAT.Rating GREEN Hash: 11A9EE1FEEC73F5258E877AB39505284
 */
public enum EArchitectureIOAttribute {

	/** Attribute 'policy'. */
	POLICY,

	/** Attribute 'name'. */
	NAME,

	/** Attribute 'regex'. */
	REGEX,

	/** Attribute 'pos'. Used for x/y Position of Components */
	POS,

	/** Attribute 'dim'. Used for h/w Size of Components */
	DIM,

	/** Attribute 'type'. */
	TYPE,

	/** Source attribute */
	SOURCE,

	/** Stereotype attribute */
	STEREOTYPE,

	/** Target attribute */
	TARGET,

	/**
	 * Points used for layouting a policy as a comma-separated sequence of
	 * integers, where each consecutive pair represents one point. Example:
	 * points="427,159,567,246,684,338" means {(427,159),(567,246),(684,338)}
	 */
	POINTS,

	/** Type attribute */
	POLICY_TYPE,

	/** AssessmentType attribute */
	ASSESSMENT_TYPE,

	/** Element used for adding XML namespace. */
	XMLNS,

	/**
	 * Optional attribute to define the included elements for the architecture
	 * scope.
	 */
	SCOPE_INCLUDE,

	/**
	 * Optional attribute to define the excluded elements for the architecture
	 * scope.
	 */
	SCOPE_EXCLUDE,

	/**
	 * Optional attribute denoting whether the architecture is file-based (as
	 * opposed to type-based)
	 */
	FILE_BASED;

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return name().toLowerCase().replace('_', '-');
	}
}