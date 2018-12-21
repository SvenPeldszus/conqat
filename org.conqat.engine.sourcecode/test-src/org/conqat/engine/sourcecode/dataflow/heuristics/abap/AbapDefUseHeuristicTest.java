/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapDefUseHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.controlflow.VariableWrite;
import org.conqat.engine.sourcecode.controlflow.VariableWrite.EVariabeWriteType;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the def-use heuristic for abap.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: 6DDB0F2739F5697E6829A76026510EAD
 */
public class AbapDefUseHeuristicTest extends TokenTestCaseBase {

	/** Tests variable definitions. */
	public void testDefinitions() throws IOException {
		assertEmptyDefinition("data ax type i.", "ax");
		assertDefaultInitialization(
				"data source_piece type ref to zcl_something.", "source_piece");
		assertNullDefinition("field-symbols <l> type i.", "<l>");
		assertDefaultInitialization("field-symbols <l> type i.", "<l>");

		assertValueDefinition("data ax type i value '1'.", "ax", "'1'");
		assertVariableDefinition("field-symbols <a> structure b default c.",
				"<a>", "c");
		assertOtherDefinition("statics ax type i value '1'.", "ax");

		assertEmptyDefinition("data(l) = '1'.", "l");
		assertValueAssignment("data(l) = '1'.", "l", "'1'");
		assertEmptyDefinition("field-symbol(<l>) = '1'.", "<l>");
		assertValueAssignment("field-symbol(<l>) = '1'.", "<l>", "'1'");

		assertNoDefinition("constants ax type i value '1'.");
		assertNoDefinition("tables l.");
		assertNoDefinition("parameters l type i");
		assertNoDefinition("types l type i.");
		assertNoDefinition("nodes l type i.");
		assertNoDefinition("field-groups l.");

		assertReads("data ax like b.", "b");
		assertReads("data ax like z.");
		assertReads("data ax like table of b.", "b");
		assertReads("data ax like range of b.", "b");
		assertReads("data ax like line of b.", "b");
	}

	/** Tests anything that's necessary for the null pointer analysis. */
	public void testNullPointerRelatedStatements() throws IOException {
		assertNullDefinition("data ax type ref to string", "ax");
		assertNullDefinition("field-symbols <ax>", "<ax>");

		assertNullAssignment("unassign <a>.", "<a>");
		assertNullAssignment("clear ra", "ra");

		assertValueAssignment("assign '1' to <a>", "<a>", "'1'");
		assertVariableAssignment("assign a to <a>", "<a>", "a");
		assertVariableAssignment("assign ra->* to <a>", "<a>", "ra");

		assertValueAssignment("get reference of '1' into ra", "ra", "'1'");
		assertVariableAssignment("get reference of a into ra", "ra", "a");

		assertValueAssignment("create data ra type string", "ra", "new");
		assertValueAssignment("create data ra like b", "ra", "new");
		assertValueAssignment("create object ra type string", "ra", "new");
		assertValueAssignment("create object ra like b", "ra", "new");

		assertDereferences("ra->foo( )", "ra");
		assertDereferences("write <a>", "<a>");
	}

	/**
	 * Tests the parts of field symbol semantics that are relevant to the
	 * null-pointer analysis.
	 */
	public void testFieldSymbols() throws IOException {
		assertNullDefinition("field-symbols <a>.", "<a>");
		assertVariableDefinition("field-symbols <a> structure b default c.",
				"<a>", "c");
		assertVariableAssignment("<a> = b", "<a>", "b");
		assertVariableAssignment("assign b to <a>", "<a>", "b");
		assertNullAssignment("unassign <a>", "<a>");
	}

	/**
	 * Tests all statements that assign variables according to
	 * 
	 * 
	 * http://help.sap.com/abapdocu_740/en/abenabap_statements_overview.htm
	 */
	public void testAssignments() throws IOException {
		assertValueAssignment("create data a type b", "a", "new");
		assertValueAssignment("create object a type b", "a", "new");
		assertAssignments("call transaction a messages into b", "b");
		assertAssignments("cleanup into a", "a");
		assertVariableAssignment("move-corresponding a to b", "b", "a");
		assertVariableAssignment("move-corresponding exact a ?to b", "b", "a");
		assertVariableAssignment("move a to b", "b", "a");
		assertValueAssignment("move 1 to b", "b", "1");
		assertAssignments("move l=>something() to b", "b");
		assertVariableAssignment("move exact a ?to b", "b", "a");
		assertVariableAssignment("unpack a to b", "b", "a");
		assertValueAssignment("unpack 'uiae' to b", "b", "'uiae'");
		assertVariableAssignment("assign a->* to <b>", "<b>", "a");
		assertValueAssignment("assign 12 to <b>", "<b>", "12");
		assertAssignments("assign a increment b to <c>", "<c>");
		assertAssignments("assign a key b index c to <d>", "<d>");
		assertNullAssignment("unassign <a>", "<a>");
		assertVariableAssignment("get reference of a into b", "b", "a");
		assertValueAssignment("get reference of 2 into b", "b", "2");
		assertVariableAssignment("clear a with b", "a", "b");
		assertValueAssignment("clear a with null", "a", "null");
		assertValueAssignment("clear a with 'b'", "a", "'b'");
		assertAssignments("clear a", "a");
		assertAssignments("clear <a>");
		assertAssignments("free a", "a");
		assertAssignments("add a to b", "b");
		assertAssignments("subtract a from b", "b");
		assertAssignments("multiply a by b", "a");
		assertAssignments("divide a by b", "a");
		assertAssignments("add-corresponding a to b", "b");
		assertAssignments("subtract-corresponding a from b", "b");
		assertAssignments("multiply-corresponding a by b", "a");
		assertAssignments("divide-corresponding a by b", "a");
		assertAssignments("concatenate a b c into d", "d");
		assertAssignments("concatenate lines of a into b", "b");
		assertAssignments("condense a", "a");
		assertReads("condense a", "a");
		assertAssignments("convert text a into sortable code b", "b");
		assertAssignments(
				"find first occurrence of x in a match count b match offset c match length d submatches e f g",
				"b", "c", "d", "e", "f", "g");
		assertAssignments(
				"find first occurrence of x in a results b submatches e f g",
				"b", "e", "f", "g");
		assertAssignments("get bit a of b into c", "c");
		assertAssignments("overlay a with b", "a");
		assertAssignments("replace 'include' in line with ''");
		assertAssignments(
				"replace first occurrence of x in a with b replacement count c replacement offset d replacement length e",
				"c", "d", "e");
		assertAssignments(
				"replace first occurrence of x in a with b replacement count c results e",
				"c", "e");
		assertAssignments("set bit a of b", "b");
		assertAssignments("shift a by b places", "a");
		assertAssignments("split a at b into c d e", "c", "d", "e");
		assertAssignments("split a at b into table c");
		assertReads("split a at b into table c", "c", "a", "b");
		assertAssignments("translate a to upper case", "a");
		assertReads("translate a to upper case", "a");
		assertAssignments("write a to b", "b");
		assertAssignments("append a to b assigning <c>", "<c>");
		assertAssignments("append a to b reference into c", "c");
		assertAssignments("loop at a");
		assertOtherAssignment("loop at a assigning <c>", "<c>");
		assertOtherAssignment("loop at <foo> assigning <c>", "<c>");
		assertAssignments(
				"LOOP AT me->package_objects ASSIGNING <a> WHERE object = 'DTEL' OR object = 'METH'",
				"<a>");
		assertAssignments("loop at a reference into c", "c");
		assertAssignments("loop at a into c", "c");
		assertAssignments("loop at a->b into c", "c");
		assertAssignments("collect a into b assigning <c>", "<c>");
		assertAssignments("collect a into b reference into c", "c");
		assertAssignments(
				"find first occurrence of x in table a match count b match line x match offset c match length d submatches e f g",
				"b", "c", "d", "e", "f", "g");
		assertAssignments(
				"find first occurrence of x in table a results b submatches e f g",
				"b", "e", "f", "g");
		assertAssignments("insert a into b assigning <c>", "<c>");
		assertAssignments("insert a into b");
		assertAssignments(
				"insert lines of a from 1 to 2 into table b reference into c",
				"c");
		assertAssignments("modify table a from b assigning <c> ", "<c>");
		assertAssignments("modify a index b reference into c", "c");
		assertAssignments("read table a assigning <c> ", "<c>");
		assertAssignments("read table a reference into c", "c");
		assertAssignments("read table a into c", "c");
		assertAssignments(
				"replace first occurrence of x in table a with b replacement count c replacement line x replacement offset d replacement length e",
				"c", "d", "e");
		assertAssignments(
				"replace first occurrence of x in table a with b replacement count c results e",
				"c", "e");
		assertReads(
				"replace first occurrence of x in table a with b replacement count c results e",
				"a", "b");
		assertAssignments("extract a", "a");
		assertAssignments(
				"describe field a type b components c length d in byte mode decimals e output-length f help-id g edit mask h",
				"b", "c", "d", "e", "f", "g", "h");
		assertAssignments("describe table a kind b lines c occurs d", "b", "c",
				"d");
		assertAssignments(
				"describe distance between a and b into c in character mode",
				"c");
		assertAssignments("get cursor line a value b length c memory offset d",
				"a", "b", "c", "d");
		assertAssignments(
				"get cursor field a value b length c offset d line e area f",
				"a", "b", "c", "d", "e", "f");
		assertAssignments("get pf-status a program b exclude c", "a", "b", "c");
		assertAssignments("loop at screen into a", "a");
		assertAssignments("describe list number of lines a", "a");
		assertAssignments("describe list number of pages a", "a");
		assertAssignments("describe list line a page b", "a", "b");
		assertAssignments(
				"describe list page a line-size b line-count c lines d first-line e top-lines f title-lines g head-lines h end-lines i",
				"a", "b", "c", "d", "e", "f", "g", "h", "i");
		assertAssignments("read line a of page b line value into c", "c");
		assertAssignments("read line a");
		assertAssignments(
				"read line a of page b field value c into d e f into g h i",
				"d", "e", "g", "h", "i");
		assertAssignments("message a into b", "b");
		assertAssignments("fetch next cursor a into b", "b");
		assertAssignments(
				"fetch next cursor a appending corresponding fields of b", "b");
		assertAssignments("fetch next cursor a into (b, c, d, e)", "b", "c",
				"d", "e");
		assertAssignments("fetch next cursor a appending b", "b");
		assertAssignments("fetch next cursor a into table b");
		assertAssignments("open cursor a for select b from c", "a");
		assertAssignments("open cursor with hold a for select b from c", "a");
		assertAssignments("select a from b into c", "c");
		assertAssignments("select a from b into corresponding fields of c", "c");
		assertAssignments("select a from b appending table c");
		assertAssignments("select a from b into (c, d, e)", "c", "d", "e");
		assertAssignments("SELECT SINGLE * INTO xdokhl FROM c WHERE id = a AND object = b.");
		assertAssignments("get dataset a position b attributes c", "b", "c");
		assertAssignments(
				"read dataset a into b maximum length c actual length d", "b",
				"d");
		assertAssignments("get parameter id a field b", "b");
		assertAssignments("get locale language a country b modifier c", "a",
				"b", "c");
		assertAssignments(
				"convert date a time b daylight saving time c into time stamp d time zone e",
				"d", "e");
		assertAssignments(
				"convert time stamp a time zone b into date c time d daylight saving time e",
				"c", "d", "e");
		assertAssignments("get time field a", "a");
		assertAssignments("get time stamp field a", "a");
		assertAssignments(
				"generate subroutine pool a name b message c include d line e word f offset g message-id h shortdump-id i",
				"b", "c", "d", "e", "f", "g", "h", "i");
		assertAssignments("insert report a from b maximum width into c", "c");
		assertAssignments("read report a into b maximum width into c", "b", "c");
		assertAssignments("read textpool a into b language c", "b");
		assertAssignments(
				"syntax-check for a message b line c word d program e directory entry f include g offset h message-id i",
				"b", "c", "d", "g", "h", "i");
		assertValueAssignment("create object a b", "a", "new");
		assertAssignments("free object a", "a");
		assertAssignments("get property of a b = c", "c");
		assertAssignments("get badi a", "a");
		assertAssignments("call transformation a source b result xml c", "c");

		assertValueAssignment("catch ZCX_SAPLINK into a", "a", "exception");
	}

	/** Tests calling methods. */
	public void testMethodCalls() throws IOException {
		assertNoAssignment("meth(a)");
		assertNoAssignment("meth(a = a)");
		assertNoAssignment("meth(exporting a = a)");
		assertAssignments("b = meth(a)", "b");
		assertAssignments("b = meth(a = a)", "b");
		assertAssignments("b = meth(exporting a = a)", "b");
		assertAssignments("meth(importing a = <a>)", "<a>");
		assertAssignments("meth(changing a = <a>)", "<a>");
		assertAssignments("meth(receiving a = <a>)", "<a>");
		assertAssignments("meth(importing a = a)", "a");
		assertAssignments("meth(changing a = a)", "a");
		assertAssignments("meth(receiving a = a)", "a");
		assertAssignments(
				"meth(receiving a = a changing b = <b> importing c = c)", "a",
				"<b>", "c");

		// weird method calls with variable that have keyword names
		assertAssignments("meth(changing a = class b = b)", "b");
		assertAssignments("meth(importing a = <class> b = b)", "b");
		assertAssignments("meth(importing class = b)", "b");
	}

	/** Tests assignment chains. */
	public void testAssignmentChains() throws IOException {
		assertAssignmentChain("a = b = c ?= d = e + 2", "a", "b", "b", "c",
				"c", "d", "d", null);
		assertAssignmentChain("a = b = c = d = e", "a", "b", "b", "c", "c",
				"d", "d", "e");
		assertAssignmentChain("a = <b> ?= c = <d> = e", "a", "<b>", "<b>", "c",
				"c", "<d>", "<d>", "e");

		assertValueAssignment("<a> = '1'.", "<a>", "'1'");
	}

	/** Tests variable reads. */
	public void testReads() throws IOException {
		assertReads("if a == 2.", "a");
		assertReads("if a == <b>.", "a", "<b>");
		assertReads("free object a");
		assertReads("select a from b into c", "a", "b");
		assertReads("a = b = c = d + e + 1", "b", "c", "d", "e");
		assertReads("modify table a from <b> assigning <c> ", "a", "<b>");
		assertReads(
				"READ TABLE currentnuggets TRANSPORTING NO FIELDS WITH KEY name = a.",
				"a");
		assertReads(
				"read line a of page b field value c into d e f into g h i",
				"a", "b", "c", "f");
		assertReads("a-b", "a");
		assertReads("a->b", "a");
		assertReads("<a>->b", "<a>");
		assertReads("x = a->b()", "a");
		assertReads("a-b = x", "a");
		assertReads("a->b = x", "a");
		assertReads("if a(4) = 'test'.", "a");
		assertReads("if a > 150.", "a");
		assertReads("loop at a assigning <b>.", "a");
	}

	/** Tests variable reads/writes to unknown variables. */
	public void testOnlyKnownVariables() throws IOException {
		assertReads("if l == 2.");
		assertReads("if a == <l>.", "a");
		assertReads("free object l");
		assertAssignments("free object l");
		assertAssignments("clear l");
	}

	/** Tests variable dereferences. */
	public void testDereferences() throws IOException {
		assertDereferences("<a>-b", "<a>");
		assertDereferences("<a> = 2", "<a>");
		assertDereferences("move 2 to <a>", "<a>");
		assertDereferences("move <a> to b", "<a>");
		assertDereferences("assign b to <a>");
		assertDereferences("unassign <a>");
		assertDereferences("loop x assigning <a>");
		assertDereferences("<a>-obj_name = objectname.", "<a>");
	}

	/** Tests begin-of/end-of statements */
	public void testBeginOf() throws IOException {
		AbapDefUseHeuristic extractor = new AbapDefUseHeuristic();

		extractor.parseStatement(scan("data begin of x.", ELanguage.ABAP));
		VariableReadWriteInfo info = extractor.parseStatement(scan(
				"data i type i.", ELanguage.ABAP));
		assertEquals(0, info.getDefinitions().size());
		extractor.parseStatement(scan("data end of x.", ELanguage.ABAP));
		VariableReadWriteInfo info2 = extractor.parseStatement(scan(
				"data i type i.", ELanguage.ABAP));
		assertEquals(1, info2.getDefinitions().size());

		extractor
				.parseStatement(scan("data begin of package.", ELanguage.ABAP));
		VariableReadWriteInfo info3 = extractor.parseStatement(scan(
				"data i type i.", ELanguage.ABAP));
		assertEquals(0, info3.getDefinitions().size());
		extractor.parseStatement(scan("data end of package.", ELanguage.ABAP));
		VariableReadWriteInfo info4 = extractor.parseStatement(scan(
				"data i type i.", ELanguage.ABAP));
		assertEquals(1, info4.getDefinitions().size());
	}

	/**
	 * Asserts that the given statement parses to an empty definition of the
	 * given variable.
	 */
	private void assertDefaultInitialization(String statement, String variable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertTrue(info.getDefinitions().get(0).isDefaultInitialization());
	}

	/**
	 * Asserts that the given statement parses to an empty definition of the
	 * given variable.
	 */
	private void assertEmptyDefinition(String statement, String variable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.EMPTY, info.getDefinitions().get(0)
				.getType());
	}

	/**
	 * Asserts that the given statement parses to an "other" definition of the
	 * given variable.
	 */
	private void assertOtherDefinition(String statement, String variable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.OTHER, info.getDefinitions().get(0)
				.getType());
	}

	/**
	 * Asserts that the given statement parses to a default initialization and
	 * null definition of the given variable.
	 */
	private void assertNullDefinition(String statement, String variable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.NULL, info.getDefinitions().get(0)
				.getType());
		assertTrue(info.getDefinitions().get(0).isDefaultInitialization());
	}

	/**
	 * Asserts that the given statement parses to a value definition of the
	 * given variable.
	 */
	private void assertValueDefinition(String statement, String variable,
			String value) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.VALUE, info.getDefinitions().get(0)
				.getType());
		assertEquals(value, info.getDefinitions().get(0).getAssignedValue());
	}

	/**
	 * Asserts that the given statement parses to a variable definition of the
	 * given variable.
	 */
	private void assertVariableDefinition(String statement, String variable,
			String assignedVariable) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getDefinitions().size());
		assertEquals(variable, info.getDefinitions().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.VARIABLE, info.getDefinitions().get(0)
				.getType());
		assertEquals(assignedVariable, info.getDefinitions().get(0)
				.getAssignedVariable());
	}

	/**
	 * Asserts that the given statement parses to no variable assignments.
	 */
	private void assertNoAssignment(String statement) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(0, info.getAssignments().size());
	}

	/**
	 * Asserts that the given statement parses to no variable definition.
	 */
	private void assertNoDefinition(String statement) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(0, info.getDefinitions().size());
	}

	/**
	 * Asserts that the given statement parses to the given dereferenced
	 * variables.
	 */
	private void assertDereferences(String statement,
			String... dereferencedVariables) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(new HashSet<String>(Arrays.asList(dereferencedVariables)),
				info.getDereferences());
	}

	/**
	 * Asserts that the given statement parses to the given read variables.
	 */
	private void assertReads(String statement, String... readVariables)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(new HashSet<String>(Arrays.asList(readVariables)),
				info.getReads());
	}

	/**
	 * Asserts that the given statement parses into several assignments of a
	 * variable to a variable.
	 * 
	 * @param variables
	 *            an even number of variable names, representing pairs of
	 *            assigned variables (first = second). If the second entry in
	 *            the pair is <code>null</code>, the assignment is expected to
	 *            be of type {@link EVariabeWriteType#OTHER}.
	 */
	private void assertAssignmentChain(String statement, String... variables)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		Set<VariableWrite> expectedWrites = new HashSet<VariableWrite>();
		CCSMAssert.isTrue(variables.length % 2 == 0,
				"Expected an even number of variables");
		for (int i = 0; i < variables.length; i += 2) {
			VariableWrite write = new VariableWrite(variables[i]);
			if (variables[i + 1] != null) {
				write.setVariable(variables[i + 1]);
			}
			expectedWrites.add(write);
		}
		assertEquals(expectedWrites,
				new HashSet<VariableWrite>(info.getAssignments()));
	}

	/**
	 * Asserts that the given statement is parsed into exactly one assignment of
	 * a value to a variable.
	 */
	private void assertValueAssignment(String statement, String variable,
			String value) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getAssignments().size());
		assertEquals(variable, info.getAssignments().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.VALUE, info.getAssignments().get(0)
				.getType());
		assertEquals(value, info.getAssignments().get(0).getAssignedValue());
	}

	/**
	 * Extracts the {@link VariableReadWriteInfo} from the given statement.
	 * 
	 * The statement is run in a context which defines the variables a to i and
	 * the field symbols <a> to <i>.
	 */
	private VariableReadWriteInfo extractInfo(String statement)
			throws IOException {
		AbapDefUseHeuristic extractor = new AbapDefUseHeuristic();
		for (char variable : "abcdefghi".toCharArray()) {
			parseDefinition(extractor, "data " + variable + " type i.");
			parseDefinition(extractor, "field-symbols <" + variable
					+ "> type i.");
			parseDefinition(extractor, "data r" + variable + " type ref to i.");
		}
		List<IToken> tokens = scan(statement, ELanguage.ABAP);
		VariableReadWriteInfo info = extractor.parseStatement(tokens);
		return info;
	}

	/**
	 * Parses the give definition statement using the extractor.
	 */
	private void parseDefinition(AbapDefUseHeuristic extractor,
			String definitionStatement) throws IOException {
		List<IToken> declarationTokens = scan(definitionStatement,
				ELanguage.ABAP);
		extractor.parseStatement(declarationTokens);
	}

	/**
	 * Asserts that the given statement parses to exactly one assignment of a
	 * variable to another variable.
	 */
	private void assertVariableAssignment(String statement,
			String changedVariable, String assignedVariable) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getAssignments().size());
		assertEquals(changedVariable, info.getAssignments().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.VARIABLE, info.getAssignments().get(0)
				.getType());
		assertEquals(assignedVariable, info.getAssignments().get(0)
				.getAssignedVariable());
	}

	/**
	 * Asserts that the given statement parses to exactly one assignment of null
	 * to another variable.
	 */
	private void assertNullAssignment(String statement, String changedVariable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getAssignments().size());
		assertEquals(changedVariable, info.getAssignments().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.NULL, info.getAssignments().get(0)
				.getType());
		assertFalse(info.getAssignments().get(0).isDefaultInitialization());
	}

	/**
	 * Asserts that the given statement parses to exactly one assignment of
	 * "other" to another variable.
	 */
	private void assertOtherAssignment(String statement, String changedVariable)
			throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		assertEquals(1, info.getAssignments().size());
		assertEquals(changedVariable, info.getAssignments().get(0)
				.getChangedVariable());
		assertEquals(EVariabeWriteType.OTHER, info.getAssignments().get(0)
				.getType());
		assertFalse(info.getAssignments().get(0).isDefaultInitialization());
	}

	/**
	 * Asserts that the given statement parses to multiple "other" assignments
	 * to the given variables.
	 */
	private void assertAssignments(String statement,
			String... assignedIdentifiers) throws IOException {
		VariableReadWriteInfo info = extractInfo(statement);
		Set<String> actuallyAssignedIdentifers = new HashSet<String>();
		for (VariableWrite assignment : info.getAllWrites()) {
			actuallyAssignedIdentifers.add(assignment.getChangedVariable());
			assertEquals(EVariabeWriteType.OTHER, assignment.getType());
		}
		assertEquals(new HashSet<String>(Arrays.asList(assignedIdentifiers)),
				actuallyAssignedIdentifers);
	}
}
