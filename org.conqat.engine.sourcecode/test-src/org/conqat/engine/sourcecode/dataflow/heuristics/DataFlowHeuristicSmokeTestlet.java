/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DataFlowHeuristicSmokeTestlet.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.io.File;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.test.TestletBase;
import org.conqat.lib.scanner.ELanguage;
import org.junit.Ignore;

/**
 * Performs a smoke test for a single file in a certain language.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: 71ED72C36979F19CCE78303D541D2245
 */
@Ignore
public class DataFlowHeuristicSmokeTestlet extends TestletBase {

	/** The input for the smoke test. */
	private final File smokeTestFile;

	/** The language of the test file. */
	private final ELanguage language;

	/** Constructor. */
	public DataFlowHeuristicSmokeTestlet(File smokeTestFile, ELanguage language) {
		this.smokeTestFile = smokeTestFile;
		this.language = language;
	}

	/** Return name of test input file as name of JUnit test */
	@Override
	public String getName() {
		return smokeTestFile.getName();
	}

	/** {@inheritDoc} */
	@Override
	public void test() throws Exception {
		String contents = FileSystemUtils.readFile(smokeTestFile);
		List<ShallowEntity> entities = TokenTestCaseBase.parseTopLevel(
				contents, language);
		IDataFlowHeuristic heuristic = DataFlowHeuristicFactory
				.createDataFlowHeuristic(language);
		PairList<String, List<ShallowEntity>> methods = heuristic
				.extractExecutables(entities);
		CCSMAssert.isFalse(methods.isEmpty(), "The file " + getName()
				+ " did not contain any methods.");

		for (int i = 0; i < methods.size(); i++) {
			List<ShallowEntity> method = methods.getSecond(i);
			try {
				TestLogger logger = new TestLogger();
				heuristic.createControlFlow(method, "", logger);
				logger.assertNoErrorsOccurred();
			} catch (Throwable e) {
				throw new ConQATException("Failed to parse file "
						+ smokeTestFile.getName(), e);
			}
		}
	}
}
