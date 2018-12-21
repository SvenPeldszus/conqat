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
package org.conqat.engine.code_clones.normalization.statement;

import java.io.Serializable;

import org.conqat.engine.code_clones.core.CloneDetectionException;
import org.conqat.engine.code_clones.core.Unit;
import org.conqat.engine.code_clones.normalization.UnitProviderBase;
import org.conqat.engine.sourcecode.resource.ITokenResource;

/**
 * Base class for statement filters.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49940 $
 * @ConQAT.Rating GREEN Hash: 40D0247E8596ED7183338D1F0A394AC4
 */
public abstract class StatementFilterBase extends
		UnitProviderBase<ITokenResource, Unit> implements Serializable {

	/** Version used for serialization. */
	private static final long serialVersionUID = 1;

	/** The provider we delegate to. */
	private final UnitProviderBase<ITokenResource, Unit> provider;

	/** The previous unit, used to correct the unit index. */
	private Unit previousUnit;

	/** Constructor. */
	protected StatementFilterBase(
			UnitProviderBase<ITokenResource, Unit> provider) {
		this.provider = provider;
	}

	/** {@inheritDoc} */
	@Override
	protected void init(ITokenResource root) throws CloneDetectionException {
		provider.init(root, getLogger());
		previousUnit = null;
	}

	/** {@inheritDoc} */
	@Override
	protected Unit provideNext() throws CloneDetectionException {
		Unit unit;
		do {
			unit = provider.getNext();
		} while (unit != null && isFiltered(unit));

		if (unit != null) {
			if (previousUnit == null || !previousUnit.inSameElement(unit)) {
				unit.setIndexInElement(0);
			} else {
				unit.setIndexInElement(previousUnit.getIndexInElement() + 1);
			}
		}

		previousUnit = unit;
		return unit;
	}

	/**
	 * Template method used for filtering. Return true if the unit should be
	 * filtered (discarded).
	 */
	protected abstract boolean isFiltered(Unit unit);

}