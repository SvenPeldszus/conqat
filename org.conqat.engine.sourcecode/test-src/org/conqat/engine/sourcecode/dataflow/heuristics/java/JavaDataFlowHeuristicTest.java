/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaDataFlowHeuristicTest.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.controlflow.ControlFlowGraph;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.dataflow.heuristics.AssignmentAsserter;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGAndReadWriteComparer;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGComparer;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the control flow graph creation for Java.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: E143D9D798D36ED2B48916915D01C34A
 */
public class JavaDataFlowHeuristicTest extends TokenTestCaseBase {

	/**
	 * The ID of the exit node. The value is arbitrary but kept in a constant to
	 * make the test code more readable.
	 */
	private static final int EXIT_NODE = 99;

	/** Tests an empty statement list. */
	public void testEmptyStatementList() throws ConQATException {
		ControlFlowNode root = createCFG(";");
		new CFGComparer(0).node(0, 1).node(1, EXIT_NODE)
				.assertMeetsSpecification(root);
	}

	/** Tests parsing of conditionals. */
	public void testConditionals() throws ConQATException, IOException {
		assertConditional("if (a) { }", null);
		assertConditional("while (a) { }", null);
		assertConditional("if (a == null) { }", "a");
		assertConditional("if (a != null) { }", "a");
		assertConditional("while (a == null) { }", "a");
		assertConditional("while (a != null) { }", "a");
		assertConditional2("for (Object a = foo(); a != null; a = a.succ) { }",
				"a");

		assertYesBranch("if (a) { a = 1; } else { a = 2; }", "a = 1;");
		assertYesBranch("if (a == null) { a = 1; } else { a = 2; }", "a = 1;");
		assertYesBranch("if (a != null) { a = 1; } else { a = 2; }", "a = 1;");
		assertYesBranch("while (a == null) { a = 1; }", "a = 1;");
		assertYesBranch("while (a != null) { a = 1; } a = 3;", "a = 1;");
	}

	/** Tests anonymous blocks. */
	public void testBlock() throws ConQATException {
		ControlFlowNode root = createCFG("{ a = 2; }");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);
	}

	/** Tests simple statements without any branches. */
	public void testSimpleStatementList() throws ConQATException {
		ControlFlowNode root = createCFG("int a = 0; int b = 22; int c = 1, d = 2; a = b + c - d;");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, 3).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root);
	}

	/** Tests single branches without loops. */
	public void testSingleBranches() throws ConQATException {
		ControlFlowNode root = createCFG("if (a == 1) { int b = 22; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("int a = 0; if (a == 1) { int b = 22; } int c = 1;");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, 3, 4).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root2);

		// empty blocks have a dummy statement inserted
		ControlFlowNode root3 = createCFG("if (a == 1) { }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("if (a == 1) { x = 1; } else { x = 2; } x = 3;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 4).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("if (a == 1) { x = 2; } else if (a == 3) { x = 4; } x = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 5).node(3, 4, 5)
				.node(4, 5).node(5, EXIT_NODE).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("if (a == 1) { x = 2; } else if (a == 3) { x = 4; } else { x = 6; } x = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 5).node(3, 4, 6)
				.node(4, 5).node(5, EXIT_NODE).node(6, 5)
				.assertMeetsSpecification(root6);
	}

	/** Tests nested branches without loops. */
	public void testNestedBranches() throws ConQATException {
		ControlFlowNode root = createCFG("if (a == 1) { if (a == 2) { int b = 3; } }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE)
				.node(2, 3, EXIT_NODE).node(3, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("if (a == 1) { if (a == 2) { int b = 3; } } x = 4;");
		new CFGComparer(0).node(0, 1).node(1, 2, 4).node(2, 3, 4).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("if (a == 1) { if (a == 2) { int b = 3; } x = 4; } x = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 5).node(2, 3, 4).node(3, 4)
				.node(4, 5).node(5, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("if (a == 1) { a = 6; if (a == 2) { int b = 3; } x = 4; }"
				+ " else { a = 7; } x = 5;");
		new CFGComparer(0).node(0, 1).node(1, 6, 7).node(6, 2).node(2, 3, 4)
				.node(3, 4).node(4, 5).node(7, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root4);
	}

	/** Tests simple while-related loops. */
	public void testWhileLoops() throws ConQATException {
		ControlFlowNode root = createCFG("while (a == 1) { a = 2; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("while (a == 1) { a = 2; } b = 3;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 1)
				.node(3, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("do { a = 2; } while (a == 1); b = 3;");
		new CFGComparer(0).node(0, 2).node(2, 1).node(1, 2, 3)
				.node(3, EXIT_NODE).assertMeetsSpecification(root3);

		// empty blocks have a dummy statement inserted
		ControlFlowNode root6 = createCFG("while (a == 1) { } x = 3;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 1)
				.node(3, EXIT_NODE).assertMeetsSpecification(root6);

		// empty blocks have a dummy statement inserted
		ControlFlowNode root7 = createCFG("while (a == 1) { }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root7);
	}

	/** Tests try-catch-finally blocks. */
	public void testTryCatch() throws ConQATException {
		ControlFlowNode root = createCFG("try { a = 1; } catch (Exception e) { a = 0; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(3, 4)
				.node(4, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("try { a = 1; } catch (Exception e) { a = 0; } finally { a = 2; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 5)
				.node(5, EXIT_NODE).node(3, 4).node(4, 5)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("try { a = 2; return 3; } catch (Exception e) { a = 4; } finally { a = 5; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 4).node(2, 3).node(3, 5)
				.node(4, 9).node(9, 5).node(5, EXIT_NODE, EXIT_NODE)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("try { a = 2; return 3; } catch (Exception e) { a = 4; return 6; } finally { a = 5; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 4).node(4, 9).node(2, 3)
				.node(5, EXIT_NODE).node(3, 5).node(9, 6).node(6, 5)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("try { a = 2; } catch (Exception e) { throw 4; } finally { if (a == 5) { a = 6; } if (a == 7) { a = 8; }}");
		new CFGComparer(0).node(0, 1).node(1, 2, 9).node(2, 5).node(9, 4)
				.node(4, 5).node(5, 6, 7).node(6, 7)
				.node(7, 8, EXIT_NODE, EXIT_NODE).node(8, EXIT_NODE, EXIT_NODE)
				.assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("try { a = 2; } catch (Exception e) { a = 3; throw 4; } finally { if (a == 5) { a = 6; } if (a == 7) { a = 8; }}");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 5).node(3, 9)
				.node(9, 4).node(4, 5).node(5, 6, 7).node(6, 7)
				.node(7, 8, EXIT_NODE, EXIT_NODE).node(8, EXIT_NODE, EXIT_NODE)
				.assertMeetsSpecification(root6);

		ControlFlowNode root7 = createCFG("try { if (2) throw 3; a = 4; return 5; } finally {}");
		new CFGComparer(0).node(0, 1).node(1, 2, 9).node(2, 3, 4).node(4, 5)
				.node(5, 9).node(3, 9).node(9, EXIT_NODE)
				.assertMeetsSpecification(root7);

		ControlFlowNode root8 = createCFG("try { a = 2; return 3; } catch (Exception e) { a = 4; } finally { a = 5; } a = 6;");
		new CFGComparer(0).node(0, 1).node(1, 2, 9).node(9, 4).node(2, 3)
				.node(3, 5).node(4, 5).node(5, 6, EXIT_NODE).node(6, EXIT_NODE)
				.assertMeetsSpecification(root8);

		ControlFlowNode root9 = createCFG("try { a = 2; return 3; } catch (Exception e) { return 4; } finally { a = 5; } a = 6;");
		new CFGComparer(0).node(0, 1).node(1, 2, 9).node(9, 4).node(2, 3)
				.node(3, 5).node(4, 5).node(5, EXIT_NODE).node(6, EXIT_NODE)
				.assertMeetsSpecification(root9);
	}

	/**
	 * Tests the {@link VariableReadWriteInfo} with which the CFG is annotated.
	 */
	public void testDataFlowAnnotations() throws ConQATException {
		int initNode = -1;
		int conditionNode = -2;
		int incrementNode = -3;
		int assignmentNode = -4;

		ControlFlowNode root = createCFG("int a = 1; while (a == 2) { a = 3; }");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites(), expectWrites(), expectReads(), 1)
				.node(1, expectWrites().value("a", "1"), expectWrites(),
						expectReads(), 2)
				.node(2, expectWrites(), expectWrites(), expectReads("a"), 3,
						EXIT_NODE)
				.node(3, expectWrites(), expectWrites().value("a", "3"),
						expectReads(), 2).assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("int a = 1; while (a == 2) { int b = 3; if (b == a || d == null) { a = b; } b = c; }");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites(), expectWrites(), expectReads(), 1)
				.node(1, expectWrites().value("a", "1"), expectWrites(),
						expectReads(), 2)
				.node(2, expectWrites(), expectWrites(), expectReads("a"), 3,
						EXIT_NODE)
				.node(3, expectWrites().value("b", "3"), expectWrites(),
						expectReads(), 4)
				.node(4, expectWrites(), expectWrites(), expectReads("b", "a"),
						5, 6)
				.node(5, expectWrites(), expectWrites().variable("a", "b"),
						expectReads("b"), 6)
				.node(6, expectWrites(), expectWrites().other("b"),
						expectReads(), 2).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("int a = 1; for (int b = 2; b < 2; b++) { a = 3; }");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites(), expectWrites(), expectReads(), 1)
				.node(1, expectWrites().value("a", "1"), expectWrites(),
						expectReads(), initNode)
				.node(initNode, expectWrites().value("b", "2"), expectWrites(),
						expectReads(), conditionNode)
				.node(conditionNode, expectWrites(), expectWrites(),
						expectReads("b"), 4, EXIT_NODE)
				.node(4, expectWrites(), expectWrites().value("a", "3"),
						expectReads(), incrementNode)
				.node(incrementNode, expectWrites(), expectWrites().other("b"),
						expectReads("b"), conditionNode)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("int a = 1, c = 2; for (int b : c) { a = 3; }");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites(), expectWrites(), expectReads(), 1)
				.node(1, expectWrites().value("a", "1").value("c", "2"),
						expectWrites(), expectReads(), initNode)
				.node(initNode, expectWrites(), expectWrites(),
						expectReads("c"), conditionNode)
				.node(conditionNode, expectWrites(), expectWrites(),
						expectReads(), assignmentNode, EXIT_NODE)
				.node(assignmentNode, expectWrites().other("b"),
						expectWrites(), expectReads(), 4)
				.node(4, expectWrites(), expectWrites().value("a", "3"),
						expectReads(), conditionNode)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("int a = 1; for (int b : this.c) { a = 3; }");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites(), expectWrites(), expectReads(), 1)
				.node(1, expectWrites().value("a", "1"), expectWrites(),
						expectReads(), initNode)
				.node(initNode, expectWrites(), expectWrites(), expectReads(),
						conditionNode)
				.node(conditionNode, expectWrites(), expectWrites(),
						expectReads(), assignmentNode, EXIT_NODE)
				.node(assignmentNode, expectWrites().other("b"),
						expectWrites(), expectReads(), 4)
				.node(4, expectWrites(), expectWrites().value("a", "3"),
						expectReads(), conditionNode)
				.assertMeetsSpecification(root5);
	}

	/** Tests simple for loops. */
	public void testForLoops() throws ConQATException {
		int initNode = -1;
		int conditionNode = -2;
		int incrementNode = -3;

		ControlFlowNode root = createCFG("for (int a = 1; a < 2; a++) { a = 3; }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, incrementNode)
				.node(incrementNode, conditionNode)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("for (int a = 1; a < 2; a++) { if (a == 3) { a = 4; } }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, 4, incrementNode)
				.node(4, incrementNode).node(incrementNode, conditionNode)
				.assertMeetsSpecification(root2);
	}

	/** Tests for-each loops. */
	public void testForEachLoops() throws ConQATException {
		int initNode = -1;
		int conditionNode = -2;
		int assignmentNode = -4;

		ControlFlowNode root = createCFG("for (int a : b) { a = 3; }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, assignmentNode, EXIT_NODE)
				.node(assignmentNode, 3).node(3, conditionNode)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("for (int a : c) { if (a == 3) { a = 4; } }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, assignmentNode, EXIT_NODE)
				.node(assignmentNode, 3).node(3, 4, conditionNode)
				.node(4, conditionNode).assertMeetsSpecification(root2);
	}

	/** Tests loops with break and continue statements. */
	public void testLoopBreaks() throws ConQATException {
		ControlFlowNode root = createCFG("while (a == 1) { break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("while (a == 1) { a = 2; break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3)
				.node(3, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("while (a == 1) { a = 2; continue; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3)
				.node(3, 1).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("while (a == 1) { a = 2; continue; a = 4; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3)
				.node(3, 1).node(4, 1).assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("while (a == 1) { if (c == 2) { continue; } x = 4; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3, 4)
				.node(3, 1).node(4, 1).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("do { if (c == 2) { continue; } x = 4; } while (a == 1);");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 4).node(3, 4)
				.node(4, 1, EXIT_NODE).assertMeetsSpecification(root6);

		ControlFlowNode root7 = createCFG("do { if (c == 2) { break; } x = 4; } while (a == 1);");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, EXIT_NODE)
				.node(3, 4).node(4, 1, EXIT_NODE)
				.assertMeetsSpecification(root7);

		int initNode = -1;
		int conditionNode = -2;
		int incrementNode = -3;

		ControlFlowNode root8 = createCFG("for (int a = 1; a < 2; a++) { continue; }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, incrementNode)
				.node(incrementNode, conditionNode)
				.assertMeetsSpecification(root8);

		ControlFlowNode root9 = createCFG("for (int a = 1; a < 2; a++) { break; }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, EXIT_NODE)
				.node(incrementNode, conditionNode)
				.assertMeetsSpecification(root9);

		ControlFlowNode root10 = createCFG("for (int a = 1; a < 2; a++) { if (a == 3) { break; } }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, 4, incrementNode)
				.node(4, EXIT_NODE).node(incrementNode, conditionNode)
				.assertMeetsSpecification(root10);

		ControlFlowNode root11 = createCFG("foo: while (2) { while (3) { break foo; } }");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, 3, 5).node(3, 4, 2)
				.node(4, 5).node(5, EXIT_NODE).assertMeetsSpecification(root11);
	}

	/** Tests return and throw statements. */
	public void testReturnAndThrow() throws ConQATException {
		ControlFlowNode root = createCFG("return;");
		new CFGComparer(0).node(0, 1).node(1, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("return; a = 12;");
		new CFGComparer(0).node(0, 1).node(1, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("if (a == 1) { return; } a = 3;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, EXIT_NODE)
				.node(3, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("while (a == 1) { if (a == 2) { return; } if (a == 4) { break; } }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3, 4)
				.node(3, EXIT_NODE).node(4, 5, 1).node(5, EXIT_NODE)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("while (a == 1) { if (a == 2) { throw x; } if (a == 4) { break; } }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3, 4)
				.node(3, EXIT_NODE).node(4, 5, 1).node(5, EXIT_NODE)
				.assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("System.exit(goo(1)); foo();");
		new CFGComparer(0).node(0, 1).node(1, EXIT_NODE)
				.assertMeetsSpecification(root6);
	}

	/** Tests synchronized statements. */
	public void testSynchronized() throws ConQATException {
		ControlFlowNode root = createCFG("synchronized (b) { int a = 0; }");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);
	}

	/** Tests switch statements. */
	public void testSwitch() throws ConQATException {
		ControlFlowNode root = createCFG("switch (a) { case 2: a = 2; break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3)
				.node(3, EXIT_NODE).assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("switch (a) { case 2: a = 2; break; default: a = 4; break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 4).node(2, 3)
				.node(3, EXIT_NODE).node(4, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("switch (a) { case 2: a = 2; break; default: a = 4; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 4).node(2, 3)
				.node(3, EXIT_NODE).node(4, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("switch (a) { case 2: a = 2; break; case 4: a = 4; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 4, EXIT_NODE).node(2, 3)
				.node(3, EXIT_NODE).node(4, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("switch (a) { case 2: a = 2; case 3: a = 3; break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 3, EXIT_NODE).node(2, 3)
				.node(3, 4).node(4, EXIT_NODE).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("switch (a) { case 2: a = 2; default: a = 3; break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 3).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root6);

		ControlFlowNode root7 = createCFG("switch (a) { case 2: return; default: a = 3; break; } a = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, EXIT_NODE)
				.node(3, 4).node(4, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root7);

		ControlFlowNode root8 = createCFG("switch (a) { case 2: default: a = 3; break; } a = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 3).node(3, 4)
				.node(4, 5).node(5, EXIT_NODE).assertMeetsSpecification(root8);

		ControlFlowNode root9 = createCFG("switch (a) { case 2: if (a == 2) { return 6; } "
				+ "default: a = 3; break; } a = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 6, 3).node(3, 4)
				.node(4, 5).node(6, EXIT_NODE).node(5, EXIT_NODE)
				.assertMeetsSpecification(root9);

		ControlFlowNode root10 = createCFG("switch (a) { case 2: if (a == 2) { break; } "
				+ "default: a = 3; break; } a = 5;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 6, 3).node(3, 4)
				.node(4, 5).node(6, 5).node(5, EXIT_NODE)
				.assertMeetsSpecification(root10);

		ControlFlowNode root11 = createCFG("switch (1) { case 2: a = 2; default: } a = 4;");
		// the default statement generates two nodes: a synthetic node for the
		// empty statement list and the synthetic break statement
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 3).node(3, 4)
				.node(4, 5).node(5, EXIT_NODE).assertMeetsSpecification(root11);
	}

	/** Tests reading and writing to/from parameters. */
	public void testParameters() throws ConQATException {
		ControlFlowNode root = createCFG("int a", "a = 2;");
		new CFGAndReadWriteComparer(0)
				.node(0, expectWrites().other("a"), expectWrites(),
						expectReads(), 1)
				.node(1, expectWrites(), expectWrites().value("a", "2"),
						expectReads(), EXIT_NODE)
				.assertMeetsSpecification(root);
	}

	/**
	 * Tests an old bug. A continue inside an if inside a for and the if is
	 * followed by another for. This lead to the continue pointing to the wrong
	 * for node.
	 */
	public void testDoubleForContinueRegression() throws ConQATException {
		int initNode = -1;
		int conditionNode = -2;
		int incrementNode = -3;
		int initNode2 = -11;
		int conditionNode2 = -12;
		int incrementNode2 = -13;

		ControlFlowNode root8 = createCFG("for (int a = 1; a < 2; a++) { if (true) { continue; } for (int b = 0; b < 2; b++) { b = 22; } }");
		new CFGComparer(0).node(0, initNode).node(initNode, conditionNode)
				.node(conditionNode, 3, EXIT_NODE).node(3, 4, initNode2)
				.node(4, incrementNode).node(initNode2, conditionNode2)
				.node(conditionNode2, 5, incrementNode).node(5, incrementNode2)
				.node(incrementNode2, conditionNode2)
				.node(incrementNode, conditionNode)
				.assertMeetsSpecification(root8);
	}

	/**
	 * Returns a new set with the given variable names. Make test cases easier
	 * to read.
	 */
	private Set<String> expectReads(String... variables) {
		return new HashSet<String>(Arrays.asList(variables));
	}

	/**
	 * Returns a new {@link AssignmentAsserter}. Makes test cases easier to
	 * read.
	 */
	private AssignmentAsserter expectWrites() {
		return new AssignmentAsserter();
	}

	/**
	 * Returns the CFG of a method that contains the given code.
	 */
	private ControlFlowNode createCFG(String code) throws ConQATException {
		return createCFG("", code);
	}

	/**
	 * Returns the CFG of a method that contains the given code and method
	 * parameters.
	 */
	private ControlFlowNode createCFG(String parameters, String code)
			throws ConQATException {
		List<ShallowEntity> entities = parseFragment("public void test("
				+ parameters + ") { " + code + " }", ELanguage.JAVA);
		assertEquals(
				"Expecting a method to parse to exactly one entity, but got\n"
						+ entities.toString(), 1, entities.size());
		TestLogger logger = new TestLogger();
		ControlFlowGraph cfg = new JavaDataFlowHeuristic().createControlFlow(
				entities, "", logger);
		logger.assertNoErrorsOccurred();
		return cfg.getRoot();
	}

	/**
	 * Asserts that the given code parses to a conditional node with the given
	 * null-checked variable.
	 */
	private void assertConditional(String code, String nullCheckedVariable)
			throws ConQATException {
		ControlFlowNode root = createCFG("String a", code);
		ControlFlowNode condition = root.getSuccessors().get(0);
		assertTrue("node was not a conditional", condition.isConditional());
		assertEquals("wrong null-checked variable", nullCheckedVariable,
				CollectionUtils.getAny(condition.getCondition()
						.getNullCheckedVariables()));
	}

	/**
	 * Asserts that the given code parses to a node whose first child is a
	 * conditional node with the given null-checked variable.
	 */
	private void assertConditional2(String code, String nullCheckedVariable)
			throws ConQATException {
		ControlFlowNode root = createCFG(code);
		ControlFlowNode condition = root.getSuccessors().get(0).getSuccessors()
				.get(0);
		assertTrue("node was not a conditional", condition.isConditional());
		assertEquals("wrong null-checked variable", nullCheckedVariable,
				CollectionUtils.getAny(condition.getCondition()
						.getNullCheckedVariables()));
	}

	/**
	 * Asserts that the given code parses to a conditional node with the given
	 * branch code in the yes branch.
	 */
	private void assertYesBranch(String code, String yesBranchCode)
			throws ConQATException, IOException {
		String yesBranch = TokenStreamUtils.toString(scan(yesBranchCode,
				ELanguage.JAVA));
		ControlFlowNode root = createCFG(code);
		ControlFlowNode condition = root.getSuccessors().get(0);
		assertTrue("node was not a conditional", condition.isConditional());
		assertEquals("wrong branch code", yesBranch,
				TokenStreamUtils.toString(condition.getYesBranch().getTokens()));
	}

}
