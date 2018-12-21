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
package org.conqat.engine.sourcecode.coverage.volume;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map.Entry;
import java.util.Properties;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.conqat.engine.sourcecode.coverage.volume.condition.CoverableConditionDecisionProcessor;
import org.conqat.engine.sourcecode.coverage.volume.condition.CoverableConditionProcessor;
import org.conqat.engine.sourcecode.coverage.volume.condition.CoverableDecisionProcessor;
import org.conqat.engine.sourcecode.coverage.volume.condition.CoverableMCDCProcessor;
import org.conqat.engine.sourcecode.coverage.volume.condition.CoverableMultiConditionProcessor;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.filesystem.CanonicalFile;
import org.conqat.lib.commons.filesystem.FileOnlyFilter;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * This is a data driven test for the coverable volume processors.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 51023 $
 * @ConQAT.Rating GREEN Hash: B636F45343D679E47B7EED9954305AF7
 */
public class CoverableVolumeProcessorTest extends CCSMTestCaseBase {

	/** Extension of the files storing expected coverable volume. */
	private static final String EXPECTED_VOLUME_EXTENSION = "properties";

	/** Creates smoke test suite. */
	public static Test suite() throws FileNotFoundException, IOException {
		// Switch to a non-static context, so we can use useTestFile() later on
		return new CoverableVolumeProcessorTest().createSuite();
	}

	/** Creates smoke test suite */
	private TestSuite createSuite() throws FileNotFoundException, IOException {

		PairList<File, File> files = findFiles();
		TestSuite suite = new TestSuite("Coverable Volume Test ["
				+ files.size() + " test files]");

		for (int i = 0; i < files.size(); ++i) {
			CanonicalFile codeFile = new CanonicalFile(files.getFirst(i));
			File propertiesFile = files.getSecond(i);

			Properties properties = new Properties();
			try (InputStream stream = new FileInputStream(propertiesFile)) {
				properties.load(stream);
			}

			for (Entry<Object, Object> entry : properties.entrySet()) {
				String key = entry.getKey().toString();
				String value = entry.getValue().toString();

				suite.addTest(new CoverableVolumeProcessorTestlet(codeFile,
						getCoverageProcessor(key), Integer.parseInt(value)));
			}
		}

		return suite;
	}

	/**
	 * Returns the processor for a coverage name from the test data properties
	 * file.
	 */
	private Class<? extends CoverableVolumeProcessorBase> getCoverageProcessor(
			String name) {
		switch (name) {
		case "line":
			return CoverableLineProcessor.class;
		case "statement":
			return CoverableStatementProcessor.class;
		case "branch":
			return CoverableBranchProcessor.class;
		case "decision":
			return CoverableDecisionProcessor.class;
		case "condition":
			return CoverableConditionProcessor.class;
		case "multicondition":
			return CoverableMultiConditionProcessor.class;
		case "condition-decision":
			return CoverableConditionDecisionProcessor.class;
		case "mc-dc":
			return CoverableMCDCProcessor.class;
		}
		throw new AssertionError("Unknown coverage: " + name);
	}

	/**
	 * Returns all test files together with the file storing the coverable
	 * volume.
	 */
	private PairList<File, File> findFiles() {
		PairList<File, File> result = new PairList<>();

		for (File file : useTestFile(".").listFiles(new FileOnlyFilter())) {
			if (EXPECTED_VOLUME_EXTENSION.equals(FileSystemUtils
					.getFileExtension(file))) {
				continue;
			}

			File expectedVolumeFile = new File(file.getAbsolutePath() + "."
					+ EXPECTED_VOLUME_EXTENSION);
			if (expectedVolumeFile.canRead()) {
				result.add(file, expectedVolumeFile);
			}
		}
		return result;
	}

}
