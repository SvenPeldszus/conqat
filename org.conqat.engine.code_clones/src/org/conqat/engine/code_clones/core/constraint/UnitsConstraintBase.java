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
package org.conqat.engine.code_clones.core.constraint;

import java.util.List;

import org.conqat.engine.code_clones.core.Clone;
import org.conqat.engine.code_clones.core.Unit;
import org.conqat.engine.code_clones.core.utils.CloneUtils;
import org.conqat.engine.core.core.ConQATException;

/**
 * Base class for constraints that work on the units of the clones. Only works,
 * if {@link Unit}s have been stored in the clones.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 48003 $
 * @ConQAT.Rating YELLOW Hash: 031A359EE9E3D653AAA55D39530FAF8D
 */
public abstract class UnitsConstraintBase extends ConstraintBase {

	/** Retrieve units for {@link Clone} */
	protected List<? extends Unit> getUnits(Clone clone) throws ConQATException {
		List<? extends Unit> units = CloneUtils.getUnits(clone);
		checkPreconditions(units);
		return units;
	}

	/**
	 * Throws a {@link ConQATException} if the constraints usage conditions are
	 * not satisfied
	 */
	private void checkPreconditions(List<? extends Unit> units)
			throws ConQATException {
		String pleaseCheckConfig = "Please check your clone detection configuration.";
		if (units == null) {
			throw new ConQATException(
					"This constraint can only be used if the units are stored. "
							+ pleaseCheckConfig);
		}

	}

}