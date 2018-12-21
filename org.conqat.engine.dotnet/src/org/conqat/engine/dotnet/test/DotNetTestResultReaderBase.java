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
package org.conqat.engine.dotnet.test;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.sourcecode.test.TestResultReaderBase;

/**
 * Base class for parsing unit test result files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 33D0A8ECA2D0A4B98D3861633B8A8735
 */
public abstract class DotNetTestResultReaderBase extends
		TestResultReaderBase<TestRoot> {

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.util.Date", description = "The time of the test run.")
	public static final String TIME_KEY = "Time";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.Boolean", description = "Flag indicating if code coverage was enabled for a test run.")
	public static final String CODE_COVERAGE = "Code Coverage";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.String", description = "Path to the code coverage result file (relative to the working directory).")
	public static final String CODE_COVERAGE_RESULT = "Code Coverage Result";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The directory the tests were run in", type = "java.lang.String")
	public static final String WORKING_DIRECTORY_KEY = "Working Directory";

	/** Parameter object for determining the node ID separator. */
	@AConQATParameterObject
	public ResultIdStrategy resultIdStrategy = new ResultIdStrategy();

	/** Constructor. */
	public DotNetTestResultReaderBase() {
		super(new TestRoot());
	}

	/** {@inheritDoc} */
	@Override
	protected final void parseTextElement(ITextElement textElement)
			throws ConQATException {
		TestFileParser parser = createParser(textElement);
		parser.parse(root);
	}

	/** Creates a {@link TestFileParser} for the given element. */
	protected abstract TestFileParser createParser(ITextElement textElement);
}
