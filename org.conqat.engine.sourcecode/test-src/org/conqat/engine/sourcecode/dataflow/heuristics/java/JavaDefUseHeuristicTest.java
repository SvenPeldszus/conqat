/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaDefUseHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.dataflow.heuristics.AssignmentAsserter;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the class that recognizes reads/writes of identifiers.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: 61604E56461F8AC702800A56A45F9A86
 */
public class JavaDefUseHeuristicTest extends TokenTestCaseBase {

	/** Tests reading parameters. */
	public void testParameterReads() throws IOException {
		assertReadIdentifiers("int a", "a == 1", "a");
		assertReadIdentifiers("int a, int b", "a == 1 || b == 2", "a", "b");
		assertReadIdentifiers("Object a", "c++");
		assertReadIdentifiers(
				"com.foo.Bar<? extends String, Map<? super Object, Blubb>> a",
				"a == null || c++ > 1", "a");
		assertReadIdentifiers("Object[] a", "a == null", "a");
		assertReadIdentifiers("byte[] a", "a == null", "a");
		assertReadIdentifiers("byte x[]", "x == null", "x");
		assertReadIdentifiers("TreeMap<String, String[]> x", "x == null", "x");
		assertReadIdentifiers("String[][] x", "x == null", "x");
		assertReadIdentifiers("String[]... x", "x == null", "x");
		assertReadIdentifiers("int x", "this.x = x", "x");
		assertReadIdentifiers(
				"int leftComponent",
				"UIFSplitPane split = new UIFSplitPane(orientation, leftComponent, rightComponent);",
				"leftComponent");
	}

	/** Tests dereferencing variables. */
	public void testDereferences() throws IOException {
		assertDereferences("int a", "a.b", "a");
		assertDereferences("int a", "a[1]", "a");
		assertDereferences("int a", "a(2)");
		assertDereferences("int a", "++a", "a");
		assertDereferences("int a", "a--", "a");
		assertDereferences("int a", "++a(2)");
		assertDereferences("int a", "a += 22", "a");
		assertDereferences("int a, int b, int c", "a += b[c--]", "a", "b", "c");
	}

	/** Tests edge cases. */
	public void testEdgeCases() throws IOException {
		// method invocation
		assertReadIdentifiers("int a", "a()");
		assertReadIdentifiers("int a", "a.b()", "a");

		// generic method invocation
		assertReadIdentifiers("int a, int b", "a.<String>b()", "a");
		assertReadIdentifiers("int a, int b, int c", "a.c.<String>b()", "a");

		// method chains
		assertReadIdentifiers("int a, int b, int c", "a.b().c()", "a");
		assertReadIdentifiers("int a, int b, int c", "a.b().c", "a");

		// fully qualified class names and outer class "this" access
		assertReadIdentifiers("int a, int b, int c", "com.a.b.SomeClass.this.c");

		// annotations
		assertReadIdentifiers("@Test(1) int a", "a.b()", "a");
		assertWrittenIdentifiers("List a",
				"@SuppressWarnings(\"unchecked\") List<String> b = a;",
				expectAssignment().variable("b", "a"), expectAssignment());
	}

	/** Tests assignment statement parsing. */
	public void testAssignments() throws IOException {
		assertWrittenIdentifiers("int a", "a = 0", expectAssignment(),
				expectAssignment().value("a", "0"));
		assertWrittenIdentifiers("Integer a", "a = null", expectAssignment(),
				expectAssignment().nullValue("a"));
		assertWrittenIdentifiers("String a", "a = \"uiae\"",
				expectAssignment(), expectAssignment().value("a", "\"uiae\""));
		assertWrittenIdentifiers("Char a", "a = 'c'", expectAssignment(),
				expectAssignment().value("a", "'c'"));
		assertWrittenIdentifiers("double a", "a = 12.45", expectAssignment(),
				expectAssignment().value("a", "12.45"));
		assertWrittenIdentifiers("float a", "a = 12.45f", expectAssignment(),
				expectAssignment().value("a", "12.45f"));
		assertWrittenIdentifiers("long a", "a = 12l", expectAssignment(),
				expectAssignment().value("a", "12l"));
		assertWrittenIdentifiers("int a", "a = 0b001", expectAssignment(),
				expectAssignment().value("a", "0b001"));
		assertWrittenIdentifiers("int a", "a = 0xdead", expectAssignment(),
				expectAssignment().value("a", "0xdead"));
		assertWrittenIdentifiers("byte a", "a = 0xd", expectAssignment(),
				expectAssignment().value("a", "0xd"));

		assertWrittenIdentifiers("Object a", "a = new Object()",
				expectAssignment(),
				expectAssignment().value("a", VariableWrite.NEW_OBJECT_VALUE));

		assertReadAndWrittenIdentifiers("String a", "a += \"b\"",
				expectAssignment(), expectAssignment().other("a"), "a");
		assertReadAndWrittenIdentifiers("String a", "while (a = readLine()) {",
				expectAssignment(), expectAssignment().other("a"));
		assertReadAndWrittenIdentifiers("String a", "a = calculate(a)",
				expectAssignment(), expectAssignment().other("a"), "a");
	}

	/**
	 * Tests assignments and definitions to/from unknown variables and object
	 * properties.
	 */
	public void testWritesWithUnknowns() throws IOException {
		assertWrittenIdentifiers("", "a = 0xd", expectAssignment(),
				expectAssignment());
		assertWrittenIdentifiers("Object a", "a.b = 0xd", expectAssignment(),
				expectAssignment());
		assertWrittenIdentifiers("", "this.b = 0xd", expectAssignment(),
				expectAssignment());
		assertWrittenIdentifiers("int a", "a = b", expectAssignment(),
				expectAssignment().other("a"));
		assertWrittenIdentifiers("", "int a = b",
				expectAssignment().other("a"), expectAssignment());
		assertWrittenIdentifiers("int a, int b", "a.b = b", expectAssignment(),
				expectAssignment());
	}

	/**
	 * Tests self modification operators.
	 */
	public void testSelfModification() throws IOException {
		// simplest cases
		assertReadAndWrittenIdentifiers("int a", "a++", expectAssignment(),
				expectAssignment().other("a"), "a");
		assertReadAndWrittenIdentifiers("int a", "++a", expectAssignment(),
				expectAssignment().other("a"), "a");
		assertReadAndWrittenIdentifiers("int a", "++b", expectAssignment(),
				expectAssignment());

		// negative cases
		assertReadAndWrittenIdentifiers("int a", "++a.b", expectAssignment(),
				expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "++c.b", expectAssignment(),
				expectAssignment());
		assertReadAndWrittenIdentifiers("int a", "x.b().a++",
				expectAssignment(), expectAssignment());

		// complex cases
		assertReadAndWrittenIdentifiers("int a", "foo(++a)",
				expectAssignment(), expectAssignment().other("a"), "a");
		assertReadAndWrittenIdentifiers("int a", "b = foo(++a)",
				expectAssignment(), expectAssignment().other("a"), "a");
		assertReadAndWrittenIdentifiers("int a, int b", "b = foo(++a)",
				expectAssignment(), expectAssignment().other("a").other("b"),
				"a");
		assertReadAndWrittenIdentifiers("int a", "int b = foo(++a)",
				expectAssignment().other("b"), expectAssignment().other("a"),
				"a");
	}

	/** Tests definition parsing. */
	public void testDefinitions() throws IOException {
		// very basic
		assertWrittenIdentifiers("", "int ax;", expectAssignment().empty("ax"),
				expectAssignment());
		assertWrittenIdentifiers("", "int a = 0",
				expectAssignment().value("a", "0"), expectAssignment());
		assertWrittenIdentifiers("", "int a = 0;",
				expectAssignment().value("a", "0"), expectAssignment());
		assertWrittenIdentifiers("int b", "int a = b;", expectAssignment()
				.variable("a", "b"), expectAssignment());
		assertWrittenIdentifiers("int b", "int a = foo();", expectAssignment()
				.other("a"), expectAssignment());

		// literals
		assertWrittenIdentifiers("", "Integer a = null", expectAssignment()
				.nullValue("a"), expectAssignment());
		assertWrittenIdentifiers("", "String a = \"uiae\"", expectAssignment()
				.value("a", "\"uiae\""), expectAssignment());
		assertWrittenIdentifiers("", "Char a = 'c'",
				expectAssignment().value("a", "'c'"), expectAssignment());
		assertWrittenIdentifiers("", "double a = 12.45", expectAssignment()
				.value("a", "12.45"), expectAssignment());
		assertWrittenIdentifiers("", "float a = 12.45f", expectAssignment()
				.value("a", "12.45f"), expectAssignment());
		assertWrittenIdentifiers("", "long a = 12l",
				expectAssignment().value("a", "12l"), expectAssignment());
		assertWrittenIdentifiers("", "int a = 0b001",
				expectAssignment().value("a", "0b001"), expectAssignment());
		assertWrittenIdentifiers("", "int a = 0xdead", expectAssignment()
				.value("a", "0xdead"), expectAssignment());
		assertWrittenIdentifiers("", "byte a = 0xd",
				expectAssignment().value("a", "0xd"), expectAssignment());

		// simple variable assignments
		assertReadAndWrittenIdentifiers("int b, int c", "int a = b + c",
				expectAssignment().other("a"), expectAssignment(), "b", "c");

		assertReadAndWrittenIdentifiers("", "Integer a = 1, b = 2",
				expectAssignment().value("a", "1").value("b", "2"),
				expectAssignment());
		assertReadAndWrittenIdentifiers("int c, int d",
				"Integer a = c = d, b = 2",
				expectAssignment().variable("a", "c").value("b", "2"),
				expectAssignment().variable("c", "d"), "d");

		// complex assignments
		assertReadAndWrittenIdentifiers(
				"int b, int c, int d, int f, int x, int y, int z",
				"Integer a = c += d = f, b = null, x = y -= z",
				expectAssignment().variable("a", "c").nullValue("b")
						.variable("x", "y"), expectAssignment().other("c")
						.variable("d", "f").other("y"), "c", "f", "y", "z");
		assertReadAndWrittenIdentifiers(
				"int b, int c, int d, int f, int x, int y, int z",
				"Integer a = c += d *= f, b = \"uiae\", x = foo(y).r -= 2 + z",
				expectAssignment().variable("a", "c").value("b", "\"uiae\"")
						.other("x"), expectAssignment().other("c").other("d"),
				"c", "d", "f", "y", "z");
	}

	/** Tests statements that contain generics. */
	public void testGenerics() throws IOException {
		assertReadAndWrittenIdentifiers("",
				"List<String> a = new ArrayList<String>();", expectAssignment()
						.value("a", VariableWrite.NEW_OBJECT_VALUE),
				expectAssignment());
		assertReadAndWrittenIdentifiers(
				"",
				"Map<String, List<String>> a = CollectionUtils.<String, List<String>> emptyMap();",
				expectAssignment().other("a"), expectAssignment());
		assertReadAndWrittenIdentifiers(
				"",
				"Map<String, List<String>> a = CollectionUtils.<String, List<String>> emptyMap(b);",
				expectAssignment().other("a"), expectAssignment());
	}

	/**
	 * There used to be a bug that empty definitions were not added to the
	 * scope.
	 */
	public void testEmptyDefinitionRegression() throws IOException {
		TestLogger logger = new TestLogger();
		JavaDefUseHeuristic extractor = new JavaDefUseHeuristic(logger);
		extractor
				.parseStatement(scan("XmlDocuments documents;", ELanguage.JAVA));

		VariableReadWriteInfo info = extractor.parseStatement(scan(
				"documents = metaDataListDialog.getXmlDocuments();",
				ELanguage.JAVA));
		assertEquals(1, info.getAssignments().size());

		info = extractor.parseStatement(scan("System.err.println(documents);",
				ELanguage.JAVA));
		assertEquals(Collections.singleton("documents"), info.getReads());
		logger.assertNoErrorsOccurred();
	}

	/** Test control flow constructs, e.g. if or while. */
	public void testControlFlow() throws IOException {
		assertReadAndWrittenIdentifiers("int a", "while (a == 2) {",
				expectAssignment(), expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "if (a == 2) {",
				expectAssignment(), expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "switch (a) {",
				expectAssignment(), expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "synchronized (a) {",
				expectAssignment(), expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "return a;",
				expectAssignment(), expectAssignment(), "a");
		assertReadAndWrittenIdentifiers("int a", "throw a;",
				expectAssignment(), expectAssignment(), "a");
	}

	/** Test defining a variable and then using it. */
	public void testDefUse() throws IOException {
		TestLogger logger = new TestLogger();
		JavaDefUseHeuristic extractor = new JavaDefUseHeuristic(logger);
		extractor.parseStatement(scan("int a = 1;", ELanguage.JAVA));
		VariableReadWriteInfo info = extractor.parseStatement(scan(
				"if (a == 1) {", ELanguage.JAVA));
		assertEquals(Collections.singleton("a"), info.getReads());
		logger.assertNoErrorsOccurred();
	}

	/** Old bugs. */
	public void testRegression() throws IOException {
		assertReadAndWrittenIdentifiers("Foo[] buffer, int index",
				"buffer[index].reset();", expectAssignment(),
				expectAssignment(), "buffer", "index");
		assertReadAndWrittenIdentifiers(
				"",
				"return new IInterface () { public void foo() { int uiae = 12; } };",
				expectAssignment(), expectAssignment());
		assertReadAndWrittenIdentifiers(
				"int[] exceptions, int bitsPerValue, int[] buffer",
				"exceptions[ex++] = buffer[i] >>> bitsPerValue;",
				expectAssignment(), expectAssignment(), "exceptions", "buffer",
				"bitsPerValue");
		assertReadAndWrittenIdentifiers(
				"",
				"final Component oppositeComponent = event.getOppositeComponent();",
				expectAssignment().other("oppositeComponent"),
				expectAssignment());
		assertReadAndWrittenIdentifiers("", "int i, j, min, M;",
				expectAssignment().empty("i").empty("j").empty("min")
						.empty("M"), expectAssignment());
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
		JavaDefUseHeuristic extractor = new JavaDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.JAVA),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.JAVA));
		assertEquals(new HashSet<String>(Arrays.asList(expectedReads)),
				info.getReads());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts both reads and writes to/from identifiers.
	 */
	private void assertReadAndWrittenIdentifiers(String methodParameters,
			String statement, AssignmentAsserter definitionAsserter,
			AssignmentAsserter assignmentAsserter, String... expectedReads)
			throws IOException {
		assertWrittenIdentifiers(methodParameters, statement,
				definitionAsserter, assignmentAsserter);
		assertReadIdentifiers(methodParameters, statement, expectedReads);
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
		JavaDefUseHeuristic extractor = new JavaDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.JAVA),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.JAVA));

		definitionAsserter.assertMatches(info.getDefinitions());
		assignmentAsserter.assertMatches(info.getAssignments());
		logger.assertNoErrorsOccurred();
	}

	/**
	 * Asserts that parsing the given statement in the context of a method with
	 * the given parameters yields the given dereferenced variables.
	 */
	private void assertDereferences(String methodParameters, String statement,
			String... dereferencedVariables) throws IOException {
		TestLogger logger = new TestLogger();
		JavaDefUseHeuristic extractor = new JavaDefUseHeuristic(logger);
		extractor.parseParameterList(scan(methodParameters, ELanguage.JAVA),
				"testMethod");
		VariableReadWriteInfo info = extractor.parseStatement(scan(statement,
				ELanguage.JAVA));

		assertEquals(CollectionUtils.asHashSet(dereferencedVariables),
				info.getDereferences());
		logger.assertNoErrorsOccurred();
	}

}
