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
package org.conqat.engine.cpp.goanna;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.cpp.CPPReportReaderTestBase;

/**
 * Tests the {@link GoannaReportReader}.
 *
 * @author $Author: heinemann $
 * @version $Rev: 49953 $
 * @ConQAT.Rating GREEN Hash: 9186623FA6425251AF53CF896D5A59C6
 */
public class GoannaReportReaderTest extends CPPReportReaderTestBase {

	/** Tests report loading. */
	public void test() throws ConQATException {
		assertEquals(
				"Call to malloc does not contain a call to sizeof in its argument@TEST/subdir/test2.cc:5-5\n"
						+ "Pointer `c' is compared with NULL, then dereferenced@TEST/test1.cc:26-26\n"
						+ "Pointer variable `buffer' is allocated but not freed, returned or passed as an argument on all paths, causing a possible memory leak@TEST/subdir/test2.cc:5-5\n"
						+ "Variable `result' may be uninitialized@TEST/test1.cc:19-19",
				runReportReaderAndReturnFindings(GoannaReportReader.class,
						"**.xml",
						", map=(prefix='/home/hummelb/system', project=TEST)"));
	}

}
