/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DataFlowHeuristicSmokeTest.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.io.File;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.conqat.engine.core.logging.testutils.ConQATProcessorTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Smoke test for the Java data flow heuristic.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: D91F6D01388A6AC245D0A3B76297F764
 */
public class DataFlowHeuristicSmokeTest extends ConQATProcessorTestCaseBase {

	/** Create a test suite. */
	public static Test suite() throws Exception {
		return new DataFlowHeuristicSmokeTest().createSuite();
	}

	/**
	 * Creates the test suite.
	 */
	private Test createSuite() {
		TestSuite suite = new TestSuite(getClass().getSimpleName());

		for (ELanguage language : ELanguage.values()) {
			File testDirectory = useTestFile(language.name().toLowerCase());
			if (!testDirectory.exists()) {
				continue;
			}

			for (File testFile : testDirectory.listFiles()) {
				suite.addTest(new DataFlowHeuristicSmokeTestlet(testFile,
						language));
			}
		}
		return suite;
	}

}
