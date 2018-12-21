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
package org.conqat.engine.code_clones.normalization.repetition;

import java.util.LinkedList;
import java.util.Queue;

import org.conqat.engine.code_clones.core.CloneDetectionException;
import org.conqat.engine.code_clones.core.Unit;
import org.conqat.engine.code_clones.detection.SentinelUnit;
import org.conqat.engine.code_clones.normalization.UnitProviderBase;
import org.conqat.engine.code_clones.normalization.provider.IUnitProvider;
import org.conqat.engine.resource.text.ITextResource;

/**
 * A normalization stage that replaces repetitive sequences of units with a
 * sentinel.
 *
 * @author $Author: kinnen $
 * @version $Rev: 50765 $
 * @ConQAT.Rating GREEN Hash: 70C7A1434A5440B114B699CD018677A8
 */
public class RepetitiveUnitFilter extends UnitProviderBase<ITextResource, Unit> {

	/** Serial version UID. */
	private static final long serialVersionUID = 1;

	/** The unit provider whose output is filtered. */
	private final IUnitProvider<ITextResource, Unit> unitProvider;

	/** The maximal size of repetition pattern checked. */
	private final int maxRepetitionPatternSize;

	/** The minimal size of a repetition region to be excluded. */
	private final int minRepetitionRegionSize;

	/** The number of repetitive tokens that are still in the buffer. */
	private transient int remainingRepetitiveTokens = 0;

	/**
	 * Stores whether the previous returned token was a sentinel that was
	 * generated here.
	 */
	private transient boolean previousWasSentinel = false;

	/**
	 * Buffer for units from the {@link #unitProvider}. We need this to have a
	 * look-ahead to check for repetitions.
	 */
	private transient Queue<Unit> unitBuffer;

	/** Constructor. */
	/* package */RepetitiveUnitFilter(
			IUnitProvider<ITextResource, Unit> unitProvider,
			int maxRepetitionPatternSize, int minRepetitionRegionSize) {
		this.unitProvider = unitProvider;
		this.maxRepetitionPatternSize = maxRepetitionPatternSize;
		this.minRepetitionRegionSize = minRepetitionRegionSize;
	}

	/** {@inheritDoc} */
	@Override
	protected void init(ITextResource root) throws CloneDetectionException {
		unitProvider.init(root, getLogger());
		unitBuffer = new LinkedList<>();
		remainingRepetitiveTokens = 0;
		previousWasSentinel = false;
	}

	/** {@inheritDoc} */
	@Override
	protected Unit provideNext() throws CloneDetectionException {
		while (true) {
			fillUnitBuffer();

			updateRepetitiveTokens();
			Unit next = unitBuffer.poll();
			if (next == null) {
				return null;
			}

			if (remainingRepetitiveTokens > 0) {
				remainingRepetitiveTokens -= 1;
				if (!previousWasSentinel) {
					previousWasSentinel = true;
					return new SentinelUnit(next);
				}
			} else {
				previousWasSentinel = false;
				return next;
			}
		}
	}

	/**
	 * Fills the {@link #unitBuffer} from the {@link #unitProvider} to have
	 * {@link #minRepetitionRegionSize} elements (if possible).
	 */
	private void fillUnitBuffer() throws CloneDetectionException {
		Unit unit;
		while (unitBuffer.size() < minRepetitionRegionSize
				&& (unit = unitProvider.getNext()) != null) {
			unitBuffer.add(unit);
		}
	}

	/**
	 * Updates the number of repetitive tokens. This is performed by checking
	 * the {@link #unitBuffer} for repetitions.
	 */
	private void updateRepetitiveTokens() {
		if (unitBuffer.size() < minRepetitionRegionSize) {
			return;
		}

		for (int patternSize = 1; patternSize <= maxRepetitionPatternSize; ++patternSize) {
			if (hasPattern(patternSize)) {
				remainingRepetitiveTokens = unitBuffer.size();
				return;
			}
		}
	}

	/**
	 * Returns whether the units in {@link #unitBuffer} have a pattern of given
	 * size.
	 */
	private boolean hasPattern(int patternSize) {
		Queue<Unit> compareQueue = new LinkedList<>();
		for (Unit unit : unitBuffer) {
			compareQueue.add(unit);
			if (patternSize-- > 0) {
				// fill compare queue
			} else if (!unit.equals(compareQueue.poll())) {
				return false;
			}
		}
		return true;
	}
}
