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
package org.conqat.engine.dotnet.test.mstest;

import java.util.EnumSet;
import java.util.Set;

import org.conqat.engine.dotnet.test.TestRun;
import org.conqat.lib.commons.xml.ElementEnumSaxHandler.IElementEnum;

/**
 * XML element state graph for MSTest reports.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating YELLOW Hash: 40F032A665C05CD173C46F3764E54AE4
 */
public enum ETrxElement implements IElementEnum<ETrxElement> {
	// TODO (FS) please mention which element is the root element. either in the
	// class comment or that element's comment
	/**
	 * Parses the <code>TestRun</code> element to get the {@link TestRun} name.
	 */
	// TODO (FS) the comment seems outdated. this class no longer does any
	// parsing.
	TESTRUN(),

	/** Holding configuration information about a test run. */
	TESTSETTINGS {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return rootElementsWith(DEPLOYMENT, DATACOLLECTOR);
		}
	},

	/** The deployment information. Descendant of {@link #TESTSETTINGS} */
	DEPLOYMENT {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return TESTSETTINGS.nextElements();
		}
	},

	/**
	 * Test coverage data collector element for MSTest until 2010. Descendant of
	 * {@link #TESTSETTINGS}
	 */
	DATACOLLECTOR {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return TESTSETTINGS.nextElements();
		}
	},

	/** Element holding the test definitions. */
	TESTDEFINITIONS {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return rootElementsWith(UNITTEST);
		}
	},

	/**
	 * Element for a single unit test method. Descendant of
	 * {@link #TESTDEFINITIONS}.
	 */
	UNITTEST {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return EnumSet.of(TESTMETHOD);
		}
	},

	/**
	 * Element holding additional information about a test method. Descendant of
	 * {@link #UNITTEST}.
	 */
	TESTMETHOD() {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return TESTDEFINITIONS.nextElements();
		}
	},

	/** Element holding execution time information. */
	TIMES,

	/** Element holding all unit test results. */
	RESULTS {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return rootElementsWith(UNITTESTRESULT);
		}
	},

	/** The result of a unit test execution. */
	UNITTESTRESULT() {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return rootElementsWith(UNITTESTRESULT);
		}
	},

	/** The summary of the unit test result. */
	RESULTSUMMARY {
		/** {@inheritDoc} */
		@Override
		public Set<ETrxElement> nextElements() {
			return rootElementsWith(COLLECTOR);
		}
	},

	/**
	 * Data collector element for MSTest 2012 and above.
	 */
	COLLECTOR;

	// TODO (FS) I think it would be clearer if every element specified the
	// nextElements instead of some elements using this default fallback
	/** {@inheritDoc} */
	@Override
	public Set<ETrxElement> nextElements() {
		return rootElements();
	}

	/** Set of elements that are the root for parsing. */
	private static EnumSet<ETrxElement> rootElements() {
		return EnumSet.of(TESTSETTINGS, TESTDEFINITIONS, TIMES, RESULTS,
				RESULTSUMMARY);
	}

	/**
	 * Set of elements returned by {@link #rootElements()} including the
	 * specified elements.
	 */
	private static EnumSet<ETrxElement> rootElementsWith(
			ETrxElement... elements) {
		EnumSet<ETrxElement> result = rootElements();
		for (ETrxElement element : elements) {
			result.add(element);
		}

		return result;
	}
}
