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
package org.conqat.engine.cpp.pclint;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.cpp.CPPReportReaderTestBase;

/**
 * Tests the {@link PCLintReportReader}.
 *
 * @author $Author: heinemann $
 * @version $Rev: 49954 $
 * @ConQAT.Rating GREEN Hash: ACBF7BF4259D0DF5C97E543C4BC26887
 */
public class PCLintReportReaderTest extends CPPReportReaderTestBase {

	/** Tests report loading. */
	public void test() throws ConQATException {
		assertEquals(
				"Custodial pointer 'x' (line 2) has not been freed or returned@TEST/subdir/test2.cpp:6-6\n"
						+ "Size of argument no. 2 inconsistent with format@TEST/test1.cpp:5-5",
				runReportReaderAndReturnFindings(PCLintReportReader.class,
						"**.xml", ", map=(prefix='', project=TEST)"));
	}
}
