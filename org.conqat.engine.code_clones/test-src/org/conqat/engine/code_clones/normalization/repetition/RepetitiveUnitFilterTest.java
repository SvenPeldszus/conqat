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

import org.conqat.engine.code_clones.core.TokenUnit;
import org.conqat.engine.code_clones.core.Unit;
import org.conqat.engine.code_clones.detection.SentinelUnit;
import org.conqat.engine.code_clones.normalization.UnitProviderBase;
import org.conqat.engine.code_clones.normalization.provider.IUnitProvider;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.ConQATProcessorTestCaseBase;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.lib.scanner.ETokenType;

/**
 * Tests the {@link RepetitiveUnitFilter} and the
 * {@link RepetitiveUnitFilterFactory}.
 *
 * @author $Author: kinnen $
 * @version $Rev: 50694 $
 * @ConQAT.Rating GREEN Hash: 660255A610FA5111956DD96CC480592B
 */
public class RepetitiveUnitFilterTest extends ConQATProcessorTestCaseBase {

	/** Test basic filtering of repetitions. */
	public void testRepetitionFiltering() throws ConQATException {
		checkFiltering("abcAAAghij", "abcAAAghij", 2, 4);
		checkFiltering("abcAAAAghij", "abc$ghij", 2, 4);

		checkFiltering("abcABABAhij", "abcABABAhij", 1, 4);

		checkFiltering("abcABABAhij", "abc$hij", 2, 4);

		checkFiltering("abcdeABCABCABfghij", "abcde$fghij", 3, 6);

		checkFiltering("abcdeABABABCDCDfghij", "abcde$fghij", 2, 4);
		checkFiltering("abcdeABABABxyzCDCDfghij", "abcde$xyz$fghij", 2, 4);
	}

	/**
	 * Checks if filtering works as expected.
	 *
	 * @param inputUnits
	 *            describes the input units (each character is one unit).
	 * @param expectedOutputUnits
	 *            the expected output; sentinels are represented as dollar sign
	 */
	private void checkFiltering(String inputUnits, String expectedOutputUnits,
			int maxPatternSize, int minRepetitionSize) throws ConQATException {
		@SuppressWarnings("unchecked")
		IUnitProvider<ITextResource, Unit> unitProvider = (IUnitProvider<ITextResource, Unit>) executeProcessor(
				RepetitiveUnitFilterFactory.class, "(unit=(provider=",
				new TestTokenProvider(inputUnits),
				"), 'repetition-pattern-size'=(max=", maxPatternSize,
				"), 'repetition-region-size'=(min=", minRepetitionSize, "))");
		unitProvider.init(null, new LoggerMock());

		StringBuilder actualUnits = new StringBuilder();
		Unit unit;
		while ((unit = unitProvider.getNext()) != null) {
			if (unit instanceof SentinelUnit) {
				actualUnits.append("$");
			} else {
				actualUnits.append(unit.getContent());
			}
		}
		assertEquals(expectedOutputUnits, actualUnits.toString());
	}

	/** Token provider used for testing. */
	private static class TestTokenProvider extends
			UnitProviderBase<ITextResource, Unit> {

		/** Serial version UID. */
		private static final long serialVersionUID = 1L;

		/** The remaining characters to build tokens from. */
		private final Queue<Character> remainingTokens = new LinkedList<>();

		/** The offset for the generated tokens. */
		private int offset = 0;

		/** Constructor. */
		public TestTokenProvider(String inputUnits) {
			for (char c : inputUnits.toCharArray()) {
				remainingTokens.add(c);
			}
		}

		/** {@inheritDoc} */
		@Override
		protected void init(ITextResource root) {
			// does nothing
		}

		/** {@inheritDoc} */
		@Override
		protected Unit provideNext() {
			Character next = remainingTokens.poll();
			if (next == null) {
				return null;
			}
			offset += 1;
			return new TokenUnit(Character.toString(next), offset, offset + 1,
					"dummy", ETokenType.TEXT, offset);
		}
	}
}
