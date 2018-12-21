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
package org.conqat.engine.text.comments.analysis;

import java.util.HashSet;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.text.comments.Comment;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Base class for comment tests, providing functionality to extract comments.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49810 $
 * @ConQAT.Rating GREEN Hash: 74FFC15725F2FC8099F742F827A058D2
 */
public abstract class CommentTestBase extends TokenTestCaseBase {

	/** Setup initializes the bundle contexts of the text bundle. */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		createBundleContext(org.conqat.engine.text.BundleContext.class);
	}

	/**
	 * Extracts all comments in the JAVA file in the test-data folder with the
	 * given name.
	 */
	protected List<Comment> getCommentsInFile(String filename)
			throws ConQATException {
		return getCommentsInFile(filename, ELanguage.JAVA);
	}

	/**
	 * Extracts all comments in the file in the test-data folder with the given
	 * name.
	 */
	protected List<Comment> getCommentsInFile(String filename,
			ELanguage language) throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), language);
		List<IToken> tokens = CommentAnalysisBase
				.unifyMultipleSingleLineComments(element
						.getTokens(new LoggerMock()));
		return CommentExtractor.extractComments(tokens, element,
				new HashSet<ETokenType>(), false);
	}
}
