/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsDataFlowHeuristicTest.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.controlflow.ControlFlowGraph;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGComparer;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the control flow graph creation for Cs.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: F8CF9D079CF798139E577899AA8A8ED7
 */
public class CsDataFlowHeuristicTest extends TokenTestCaseBase {

	/**
	 * The ID of the exit node. The value is arbitrary but kept in a constant to
	 * make the test code more readable.
	 */
	private static final int EXIT_NODE = 99;

	/** Tests yield statements. */
	public void testYield() throws ConQATException {
		ControlFlowNode root = createCFG("while (1) { if (2) { yield return 3; } else { yield break; }");
		new CFGComparer(0).node(0, 1).node(1, 2, EXIT_NODE).node(2, 3, 4)
				.node(3, 1).node(4, EXIT_NODE).assertMeetsSpecification(root);
	}

	/** Tests goto statements. */
	public void testGoto() throws ConQATException {
		ControlFlowNode root = createCFG("foo: goto foo;");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, 1)
				.assertMeetsSpecification(root);

		ControlFlowNode root1 = createCFG("goto foo; return; foo: a = 3;");
		new CFGComparer(0).node(0, 1).node(1, 2).node(2, 3).node(3, 4)
				.node(4, EXIT_NODE).assertMeetsSpecification(root1);

		ControlFlowNode root2 = createCFG("switch (a) { case 1: goto default; default: break;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 3)
				.node(3, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("switch (a) { case 1: goto case 1; default: break;");
		new CFGComparer(0).node(0, 1).node(1, 2, 3).node(2, 2)
				.node(3, EXIT_NODE).assertMeetsSpecification(root3);
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
				+ parameters + ") { " + code + " }", ELanguage.CS);
		assertEquals(
				"Expecting a method to parse to exactly one entity, but got\n"
						+ entities.toString(), 1, entities.size());
		TestLogger logger = new TestLogger();
		ControlFlowGraph cfg = new CsDataFlowHeuristic().createControlFlow(
				entities, "", logger);
		logger.assertNoErrorsOccurred();
		return cfg.getRoot();
	}

}
