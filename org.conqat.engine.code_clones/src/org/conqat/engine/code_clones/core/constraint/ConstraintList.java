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
package org.conqat.engine.code_clones.core.constraint;

import java.util.ArrayList;

import org.conqat.engine.code_clones.core.CloneClass;
import org.conqat.engine.core.core.ConQATException;

/**
 * List of clone class constraints.
 * 
 * @author juergens
 * @author $Author: poehlmann $
 * @version $Rev: 48262 $
 * @ConQAT.Rating YELLOW Hash: E2EBDFD5096A19B9AE8118B396F65619
 */
public class ConstraintList extends ArrayList<ICloneClassConstraint> {

	/** Version used for serialization. */
	private static final long serialVersionUID = 1;

	/** Returns true, if the cloneClass satisfies all constraints, false if not */
	public boolean allSatisfied(CloneClass cloneClass) throws ConQATException {
		for (ICloneClassConstraint constraint : this) {
			if (!constraint.satisfied(cloneClass)) {
				return false;
			}
		}
		return true;
	}

	/** Returns true, if the cloneClass satisfies one constraint, false if not */
	public boolean oneSatisfied(CloneClass cloneClass) throws ConQATException {
		for (ICloneClassConstraint constraint : this) {
			if (constraint.satisfied(cloneClass)) {
				return true;
			}
		}
		return false;
	}

}