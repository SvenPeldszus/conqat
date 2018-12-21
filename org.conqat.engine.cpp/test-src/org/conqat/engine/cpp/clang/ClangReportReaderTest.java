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
package org.conqat.engine.cpp.clang;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.cpp.CPPReportReaderTestBase;

/**
 * Tests the {@link ClangReportReader}.
 *
 * @author $Author: heinemann $
 * @version $Rev: 49952 $
 * @ConQAT.Rating GREEN Hash: 4BFC7CEAF2C3E500E461B569D8B4553A
 */
public class ClangReportReaderTest extends CPPReportReaderTestBase {

	/** Tests report loading. */
	public void test() throws ConQATException {
		assertEquals(
				"Dereference of null pointer (loaded from variable 'c')@TEST/test1.cc:26-26\n"
						+ "Potential leak of memory pointed to by 'buffer'@TEST/subdir/test2.cc:9-9\n"
						+ "Undefined or garbage value returned to caller@TEST/test1.cc:19-19",
				runReportReaderAndReturnFindings(ClangReportReader.class,
						"**.plist", ", map=(prefix='', project=TEST)"));
	}
}
