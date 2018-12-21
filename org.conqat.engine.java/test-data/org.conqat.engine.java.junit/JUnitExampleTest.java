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
package org.conqat.engine.java.junit;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;

import org.conqat.lib.commons.concurrent.ThreadUtils;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * A test that intends to create a non-trivial JUnit XML.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49499 $
 * @ConQAT.Rating RED Hash:
 */
public class JUnitExampleTest {

	/** Create a test suite. */
	public static Test suite() throws Exception {
		TestSuite suite = new TestSuite("OuterSuite");

		TestSuite innerSuite = new TestSuite("InnerSuite");
		suite.addTest(innerSuite);

		TestSuite innerInnerSuite = new TestSuite("InnerInnerSuite");
		innerSuite.addTest(innerInnerSuite);

		suite.addTest(new SuccessTestlet());
		innerSuite.addTest(new ErrorTestlet());
		innerInnerSuite.addTest(new FailureTestlet());
		innerInnerSuite.addTest(new SlowTestlet());

		return suite;
	}

	/** A test that simply works. */
	public static class SuccessTestlet extends CCSMTestCaseBase {

		/** Constructor. */
		public SuccessTestlet() {
			setName("testSuccess");
		}

		/** Test method. */
		public void testSuccess() {
			// does nothing
		}
	}

	/** A slow test. */
	public static class SlowTestlet extends CCSMTestCaseBase {

		/** Constructor. */
		public SlowTestlet() {
			setName("testSlow");
		}

		/** Test method. */
		public void testSlow() {
			ThreadUtils.sleep(1000);
		}
	}

	/** A test with an error. */
	public static class ErrorTestlet extends CCSMTestCaseBase {

		/** Constructor. */
		public ErrorTestlet() {
			setName("testError");
		}

		/** Test method. */
		public void testError() {
			throw new RuntimeException("Intentional error!");
		}
	}

	/** A failing test. */
	public static class FailureTestlet extends CCSMTestCaseBase {

		/** Constructor. */
		public FailureTestlet() {
			setName("testFailure");
		}

		/** Test method. */
		public void testFailure() {
			Assert.fail("Failed intentionally.");
		}
	}

}
