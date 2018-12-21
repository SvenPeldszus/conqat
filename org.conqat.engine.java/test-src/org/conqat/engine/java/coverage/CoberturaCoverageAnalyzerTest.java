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
package org.conqat.engine.java.coverage;

/**
 * Tests for the {@link CoberturaCoverageAnalyzer}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48740 $
 * @ConQAT.Rating GREEN Hash: BA40A8215EED8B15189D816EF6C6D971
 */
public class CoberturaCoverageAnalyzerTest extends JavaCoverageAnalyzerTestBase {

	/** {@inheritDoc} */
	@Override
	protected Class<? extends JavaCoverageAnalyzerBase> getCoverageAnalyzerClass() {
		return CoberturaCoverageAnalyzer.class;
	}

	/** {@inheritDoc} */
	@Override
	protected String getSimpleReportFile() {
		return "cobertura-report.xml";
	}

}
