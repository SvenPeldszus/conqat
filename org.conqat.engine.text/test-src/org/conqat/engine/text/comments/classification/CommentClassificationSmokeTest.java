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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Data driven test for comment classification. This is used to complement the
 * machine learner with cases where we want to force/ensure the correct type.
 * Each file contains comments that are annotated with markers. Programming
 * language is detected by file extension.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 49711 $
 * @ConQAT.Rating GREEN Hash: CF5CB70A9033E43324C7BE5C83F038A5
 */
public class CommentClassificationSmokeTest extends CCSMTestCaseBase {

	/** Creates smoke test suite */
	public static Test suite() {
		// Switch to a non-static context, so we can use useTestFile() later on
		return new CommentClassificationSmokeTest().createSuite();
	}

	/** Creates smoke test suite */
	private TestSuite createSuite() {
		List<File> files = findFiles();
		TestSuite suite = new TestSuite("Shallow Parser Smoke Test ["
				+ files.size() + " test files]");

		for (File file : files) {
			suite.addTest(new CommentClassificationSmokeTestlet(file));
		}
		return suite;
	}

	/** Returns all files containing smoke test data. */
	private List<File> findFiles() {
		List<File> result = new ArrayList<>();
		for (File file : useTestFile("smoke").listFiles()) {
			if (file.isDirectory() || file.isHidden()) {
				continue;
			}
			result.add(file);
		}
		return result;
	}
}
