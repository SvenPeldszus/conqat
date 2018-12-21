/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapDataFlowHeuristicTest.java 51692 2015-02-06 09:52:21Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.TestLogger;
import org.conqat.engine.sourcecode.controlflow.ControlFlowGraph;
import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.CFGComparer;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the control flow graph creation for ABAP.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51692 $
 * @ConQAT.Rating YELLOW Hash: 379BD7BC017E022A8C03D7446719E7DC
 */
public class AbapDataFlowHeuristicTest extends TokenTestCaseBase {

	/** Used to mark the exit node in the test cases. */
	private static final Integer EXIT_NODE = -99;

	/** Tests an empty program block. */
	public void testEmptyFunctions() throws ConQATException {
		// headers (i.e. no nesting in shallow entities)
		ControlFlowNode root = createCFG("report test.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root);

		ControlFlowNode root7 = createCFG("report test. .");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root7);

		// block structures
		ControlFlowNode root2 = createCFG("form uiae. endform.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("class a implementation. method uiae. endmethod. endclass.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("function uiae. endfunction.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root4);

		// events
		ControlFlowNode root5 = createCFG("at line-selection.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("end-of-page.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root6);
	}

	/** Tests variable declaration statements. */
	public void testData() throws ConQATException {
		ControlFlowNode root = createCFG("report test. data: foo(2) type c, bar(2) like foo.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. statics: foo(2) type c, bar(2) like foo.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("report test. constants: foo(2) type c value 'ua', bar(2) like foo value 'ab'.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("report test. field-symbols: <foo> type c, <bar> like foo.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("report test. parameters: a type c, b like foo.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root5);
	}

	/** Tests statements that are simply ignored. */
	public void testIgnoredStatements() throws ConQATException {
		ControlFlowNode root = createCFG("report test. types foo like bar. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. types: begin of foo, a type c length 2, end of foo. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("report test. types begin of foo. types a type c length 2. types end of foo. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("report test. tables: foo, bar. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("report test. nodes: foo, bar. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("report test. define a. foo. bar. end-of-definition. write 'c'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root6);
	}

	/** Tests simple statements. */
	public void testStatements() throws ConQATException {
		ControlFlowNode root = createCFG("report test. write 'hello'. write 'uiae'. add 12 to sy-index.");
		new CFGComparer(1).node(1, 2).node(2, 3).node(3, EXIT_NODE)
				.assertMeetsSpecification(root);
	}

	/** Tests if statements. */
	public void testIf() throws ConQATException {
		ControlFlowNode root = createCFG("report test. if x = 0. write 'hello'. endif.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. if x = 0. write 'hello'. else. write 'hello'. endif.");
		new CFGComparer(1).node(1, 2, 3).node(2, EXIT_NODE).node(3, EXIT_NODE)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("report test. if x = 1. write 2. elseif x = 3. write 4. endif.");
		new CFGComparer(1).node(1, 2, 3).node(2, EXIT_NODE)
				.node(3, 4, EXIT_NODE).node(4, EXIT_NODE)
				.assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("report test. if x = 1. write 2. elseif x = 3. write 4. else. write 5. endif.");
		new CFGComparer(1).node(1, 2, 3).node(2, EXIT_NODE).node(3, 4, 5)
				.node(4, EXIT_NODE).node(5, EXIT_NODE)
				.assertMeetsSpecification(root4);
	}

	/** Tests compound statements, e.g. "write: 'hello', 'you'." */
	public void testCompoundStatements() throws ConQATException {
		ControlFlowNode root4 = createCFG("report test. write: 'hello', 'you'.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root4);
	}

	/** Tests while loops. */
	public void testWhile() throws ConQATException {
		ControlFlowNode root = createCFG("report test. while x = 1. write 'y'. endwhile.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root);
	}

	/** Tests do loops. */
	public void testDo() throws ConQATException {
		ControlFlowNode root = createCFG("report test. do. write 'y'. enddo.");
		new CFGComparer(1).node(1, 2).node(2, 1).assertMeetsSpecification(root);

		ControlFlowNode root1 = createCFG("report test. do 8 times. write 'y'. enddo.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root1);
	}

	/** Tests case statements. */
	public void testCase() throws ConQATException {
		ControlFlowNode root = createCFG("report test. case x. when 1. write '1'. endcase.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root1 = createCFG("report test. case x. when 1. write '1'. when others. write '2'. endcase.");
		new CFGComparer(1).node(1, 2, 3).node(2, EXIT_NODE).node(3, EXIT_NODE)
				.assertMeetsSpecification(root1);

		ControlFlowNode root2 = createCFG("report test. case x. when 1. leave program. when others. leave program. endcase. a = 2.");
		new CFGComparer(1).node(1, 2, 3).node(2, EXIT_NODE).node(3, EXIT_NODE)
				.assertMeetsSpecification(root2);
	}

	/** Tests loop and provide loops. */
	public void testTableLoops() throws ConQATException {
		ControlFlowNode root = createCFG("report test. loop at itab. write '1'. endloop.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. provide fields col3 from itab into wa1. write '1'. endprovide.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root2);
	}

	/** Tests selects and single selects. */
	public void testSelect() throws ConQATException {
		ControlFlowNode root = createCFG("report test. select col1 from itab into x. write '1'. endselect.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root);

		// single select
		ControlFlowNode root2 = createCFG("report test. select single col1 from itab into x.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root2);
	}

	/** Tests exit, check, etc. statements. */
	public void testBreaks() throws ConQATException {
		ControlFlowNode root = createCFG("report test. do. exit. enddo.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. while a = 1. exit. endwhile.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, EXIT_NODE)
				.assertMeetsSpecification(root2);

		// exit outside of a loop acts like a return
		ControlFlowNode root3 = createCFG("report test. exit. write 'a'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("report test. do. continue. enddo.");
		new CFGComparer(1).node(1, 2).node(2, 1)
				.assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("report test. while a = 1. continue. endwhile.");
		new CFGComparer(1).node(1, 2, EXIT_NODE).node(2, 1)
				.assertMeetsSpecification(root5);
	}

	/** Tests statements that end the current program. */
	public void testReturns() throws ConQATException {
		ControlFlowNode root = createCFG("report test. leave program. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. return. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("report test. reject. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root3);

		ControlFlowNode root4 = createCFG("report test. stop. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root4);

		ControlFlowNode root5 = createCFG("report test. raise x. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root5);

		ControlFlowNode root6 = createCFG("report test. raise exception x. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root6);

		ControlFlowNode root7 = createCFG("report test. submit x. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root7);

		ControlFlowNode root8 = createCFG("report test. submit x and return. write 'hello'.");
		new CFGComparer(1).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root8);

		ControlFlowNode root9 = createCFG("report test. leave to transaction SE80. write 'hello'.");
		new CFGComparer(1).node(1, EXIT_NODE).assertMeetsSpecification(root9);
	}

	/** Tests try catch statements. */
	public void testTryCatch() throws ConQATException {
		ControlFlowNode root = createCFG("report test. try. write 'hello'. catch x. write 'x'. endtry.");
		new CFGComparer(0).node(0, 1, 9).node(9, 2).node(2, EXIT_NODE)
				.node(1, EXIT_NODE).assertMeetsSpecification(root);

		ControlFlowNode root2 = createCFG("report test. try. write 'hello'. cleanup. write 'y'. endtry.");
		new CFGComparer(0).node(0, 1, 2).node(1, 2).node(2, EXIT_NODE)
				.assertMeetsSpecification(root2);

		ControlFlowNode root3 = createCFG("report test. try. write 'hello'. catch x. write 'x'. cleanup. write 'y'. endtry.");
		new CFGComparer(0).node(0, 1, 9).node(9, 2).node(1, 3)
				.node(3, EXIT_NODE).node(2, 3).assertMeetsSpecification(root3);
	}

	/**
	 * Returns the CFG of a method that contains the given code.
	 */
	private ControlFlowNode createCFG(String code) throws ConQATException {
		List<ShallowEntity> entities = parseTopLevel(code, ELanguage.ABAP);
		AbapDataFlowHeuristic heuristic = new AbapDataFlowHeuristic();
		PairList<String, List<ShallowEntity>> executables = heuristic
				.extractExecutables(entities);
		CCSMAssert.isTrue(executables.size() == 1,
				"expected to find exactly one method.");
		TestLogger logger = new TestLogger();
		ControlFlowGraph cfg = heuristic.createControlFlow(
				executables.getSecond(0), "", logger);
		logger.assertNoErrorsOccurred();
		return cfg.getRoot();
	}

}
