/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.dotnet.test.DotNetTestResultReaderBase;
import org.conqat.engine.dotnet.test.TestFileParser;
import org.conqat.engine.resource.text.ITextElement;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 8CED724407D5D8145225748F828FC895
 */
@AConQATProcessor(description = "This processor reads .trx files that contain the test results of MSTest tests")
public class TrxFileReader extends DotNetTestResultReaderBase {

	/** {@inheritDoc} */
	@Override
	protected TestFileParser createParser(ITextElement textElement) {
		return new TrxFileParser(textElement, resultIdStrategy);
	}
}