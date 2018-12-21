/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsDefUseHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.AssignmentAsserter;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the class that recognizes reads/writes of identifiers.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: 8B2D879864ECFDA9025571760D15669A
 */
public class CsDefUseHeuristicTest extends TokenTestCaseBase {

	/** Tests specialties of C#. */
	public void testSpecialities() throws IOException {
		assertWrittenIdentifiers("",
				" ZipSegmentedStream zss = new ZipSegmentedStream() { "
						+ "rwMode = RwMode.ReadOnly, "
						+ "CurrentSegment = initialDiskNumber, "
						+ "_maxDiskNumber = maxDiskNumber, "
						+ "_baseName = name, };",
				expectAssignment().value("zss", "new"), expectAssignment());

		assertParameterNames(
				"this T objectProperty, out T replaceValueIfIsNull, ref int a, ref int* b",
				"objectProperty", "replaceValueIfIsNull", "a", "b");
		assertParameterDefinitions(
				"out int a, out IList<String> b, out IFoo c",
				expectAssignment().otherDefaultInit("a").otherDefaultInit("b")
						.otherDefaultInit("c"));
	}

	/** Tests old bugs. */
	public void testRegression() throws IOException {
		assertWrittenIdentifiers("", "i < KeysPerOctave * MaxOctave;",
				expectAssignment(), expectAssignment());
		assertReadIdentifiers(
				"object visitor",
				"var expressionSubTree = expression.Expressions.Select(exp => visitor.Visit(exp));",
				"visitor");
		// did not parse at all due to the pragmas
		assertReadIdentifiers(
				"DbConnection connection, int? commandTimeout,\n#pragma warning disable 3001\nStoreItemCollection storeItemCollection)\n#pragma warning restore 3001",
				"storeItemCollection.foo();", "storeItemCollection");
	}

	/** Tests pointer syntax. */
	public void testPointers() throws IOException {
		assertDereferencedIdentifiers("int* f", "a = *f", "f");
		assertDereferencedIdentifiers("", "int* f = getF();");
	}

	/**
	 * Creates a new {@link AssignmentAsserter}. Makes the test cases less
	 * verbose.
	 */
	private AssignmentAsserter expectAssignment() {
		return new AssignmentAsserter();
	}

	/**
	 * Asserts that parsing the given statement in the context of a method with
	 * the given parameters yields the expected identifier reads.
	 */
	private void assertReadIdentifiers(String methodParameters,
			String statement, String... expectedReads) throws IOException {
		TestLogger logger = new TestLogger();
		CsDefUseHeuristic extractor = new CsDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.CS),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.CS));
		assertEquals(new HashSet<String>(Arrays.asList(expectedReads)),
				info.getReads());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts that parsing the given statement in the context of a method with
	 * the given parameters yields the expected identifier dereferences.
	 */
	private void assertDereferencedIdentifiers(String methodParameters,
			String statement, String... expectedReads) throws IOException {
		TestLogger logger = new TestLogger();
		CsDefUseHeuristic extractor = new CsDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.CS),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.CS));
		assertEquals(new HashSet<String>(Arrays.asList(expectedReads)),
				info.getDereferences());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts that the definitions and assignments obtained by parsing the
	 * given statement in the context of a method with the given parameters
	 * satisfies the given asserter.
	 */
	private void assertWrittenIdentifiers(String methodParameters,
			String statement, AssignmentAsserter definitionAsserter,
			AssignmentAsserter assignmentAsserter) throws IOException {
		TestLogger logger = new TestLogger();
		CsDefUseHeuristic extractor = new CsDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.CS),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.CS));

		definitionAsserter.assertMatches(info.getDefinitions());
		assignmentAsserter.assertMatches(info.getAssignments());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts that the definitions and assignments obtained by parsing the
	 * given statement in the context of a method with the given parameters
	 * satisfies the given asserter.
	 */
	private void assertParameterDefinitions(String methodParameters,
			AssignmentAsserter definitionAsserter) throws IOException {
		TestLogger logger = new TestLogger();
		CsDefUseHeuristic extractor = new CsDefUseHeuristic(logger);
		VariableReadWriteInfo info = extractor.parseParameterList(
				scan(methodParameters, ELanguage.CS), "testMethod");
		definitionAsserter.assertMatches(info.getDefinitions());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts that the given parameter list is parsed to definitions of the
	 * given variables.
	 */
	private void assertParameterNames(String parameterList,
			String... definedParameters) throws IOException {
		TestLogger logger = new TestLogger();
		CsDefUseHeuristic extractor = new CsDefUseHeuristic(logger);
		VariableReadWriteInfo info = extractor.parseParameterList(
				scan(parameterList, ELanguage.CS), "testMethod");
		Set<String> defs = new HashSet<String>();
		for (VariableWrite write : info.getDefinitions()) {
			defs.add(write.getChangedVariable());
		}
		assertEquals(new HashSet<>(Arrays.asList(definedParameters)), defs);
		logger.assertNoErrorsOccurred();
	}

}
