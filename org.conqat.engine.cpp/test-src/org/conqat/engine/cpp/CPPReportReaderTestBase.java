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
package org.conqat.engine.cpp;

import org.conqat.engine.sourcecode.report.ReportReaderTestBase;

/**
 * Base class for C++ report reader tests
 *
 * @author $Author: heinemann $
 * @version $Rev: 49969 $
 * @ConQAT.Rating GREEN Hash: 061A13F23A8857ED864BFBD8A747EFC0
 */
public abstract class CPPReportReaderTestBase extends ReportReaderTestBase {

	/** {@inheritDoc} */
	@Override
	protected String[] getIncludePatterns() {
		return new String[] { "**.cc", "**.cpp" };
	}

}
