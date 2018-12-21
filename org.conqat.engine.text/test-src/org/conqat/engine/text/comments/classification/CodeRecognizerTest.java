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
package org.conqat.engine.text.comments.classification;

import org.conqat.lib.scanner.ELanguage;

import junit.framework.TestCase;

/**
 * Tests for the {@link CodeRecognizer}. We only test the recognition of single
 * code lines. The testing of entire comments in context is performed in the
 * smoke test.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49709 $
 * @ConQAT.Rating GREEN Hash: 22DFC5F0127CE6859AD7B447BE88EB37
 */
public class CodeRecognizerTest extends TestCase {

	/** Test lines that should be detected as code. */
	public void testPositives() throws Exception {
		assertTrue(CodeRecognizer.isCodeLine("int x = 17;", ELanguage.JAVA));
		assertTrue(CodeRecognizer.isCodeLine(
				"Util.pr(field+\": '\"+s1+\"' vs '\"+s2+\"'\");",
				ELanguage.JAVA));
		assertTrue(CodeRecognizer.isCodeLine("virtual void reset( void ) = 0;",
				ELanguage.CPP));
	}

	/** Test lines that should not be detected as code. */
	public void testNegatives() throws Exception {
		assertFalse(CodeRecognizer.isCodeLine("old style %{ now @{",
				ELanguage.JAVA));
		assertFalse(CodeRecognizer
				.isCodeLine(
						"http://social.msdn.microsoft.com/Forums/vstudio/en-US/a20fbd0d-4cb0-481a-8aba-7f336d882f41/memory-leak-in-ms-tfs-java-sdk-10",
						ELanguage.JAVA));
		assertFalse(CodeRecognizer.isCodeLine("{@inheritDoc}", ELanguage.JAVA));
		assertFalse(CodeRecognizer.isCodeLine("just. See:", ELanguage.JAVA));
		assertFalse(CodeRecognizer.isCodeLine(
				"<summary>This is a summary</sumary>", ELanguage.CS));
		assertFalse(CodeRecognizer.isCodeLine("<pre>i += 1;</pre>",
				ELanguage.JAVA));
	}

}
