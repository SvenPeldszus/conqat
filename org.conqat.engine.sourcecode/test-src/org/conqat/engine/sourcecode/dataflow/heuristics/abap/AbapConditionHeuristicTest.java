/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: AbapConditionHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.abap;

import java.io.IOException;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the condition parsing heuristic for Java.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: 9B2D97F672BC06B1A9DE446727A6CDC0
 */
public class AbapConditionHeuristicTest extends TokenTestCaseBase {

	/** Tests null checks of a single variable. */
	public void testSingleCondition() throws IOException {
		assertCondition("if a is bound", "yes=[a=N], no=[a=A], checked=[a]");
		assertCondition("if a is bound.", "yes=[a=N], no=[a=A], checked=[a]");
		assertCondition("if a is not bound", "yes=[a=A], no=[a=N], checked=[a]");
		assertCondition("if not a is bound", "yes=[a=A], no=[a=N], checked=[a]");

		assertCondition("if <a> is assigned",
				"yes=[<a>=N], no=[<a>=A], checked=[<a>]");
		assertCondition("if <a> is assigned.",
				"yes=[<a>=N], no=[<a>=A], checked=[<a>]");
		assertCondition("if <a> is not assigned",
				"yes=[<a>=A], no=[<a>=N], checked=[<a>]");
		assertCondition("if not <a> is assigned",
				"yes=[<a>=A], no=[<a>=N], checked=[<a>]");

		assertCondition("if a is not initial",
				"yes=[a=N], no=[a=A], checked=[a]");
		assertCondition("if a is not initial.",
				"yes=[a=N], no=[a=A], checked=[a]");
		assertCondition("if a is initial", "yes=[a=A], no=[a=N], checked=[a]");
		assertCondition("if not a is initial",
				"yes=[a=N], no=[a=A], checked=[a]");
	}

	/** Tests null-checks chained with other things using and. */
	public void testAndChain() throws IOException {
		assertCondition("if <a> is not assigned and <b> is not assigned",
				"yes=[<a>=A, <b>=A], no=[], checked=[<a>, <b>]");
		assertCondition(
				"if <a> is not assigned and <b> is not assigned and <c> is not assigned",
				"yes=[<a>=A, <b>=A, <c>=A], no=[], checked=[<a>, <b>, <c>]");
		assertCondition("if <a> is assigned and <b> is assigned",
				"yes=[<a>=N, <b>=N], no=[], checked=[<a>, <b>]");
		assertCondition("if <a> is assigned and <b> is not assigned",
				"yes=[<a>=N, <b>=A], no=[], checked=[<a>, <b>]");
		assertCondition("if <a> is assigned and <a>-length > 0",
				"yes=[<a>=N], no=[], checked=[<a>]");

		assertCondition("if a is initial and b is initial",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("if a is initial and b is initial and c is initial",
				"yes=[a=A, b=A, c=A], no=[], checked=[a, b, c]");
		assertCondition("if a is not initial and b is not initial",
				"yes=[a=N, b=N], no=[], checked=[a, b]");
		assertCondition("if a is not initial and b is initial",
				"yes=[a=N, b=A], no=[], checked=[a, b]");
		assertCondition("if a is not initial and a-length > 0",
				"yes=[a=N], no=[], checked=[a]");
	}

	/** Tests null-checks chained with other things using or. */
	public void testOrChain() throws IOException {
		assertCondition("if <a> is not assigned or <b> is not assigned",
				"yes=[], no=[<a>=N, <b>=N], checked=[<a>, <b>]");
		assertCondition(
				"if <a> is not assigned or <b> is not assigned or <c> is not assigned",
				"yes=[], no=[<a>=N, <b>=N, <c>=N], checked=[<a>, <b>, <c>]");
		assertCondition("if <a> is assigned or <b> is assigned",
				"yes=[], no=[<a>=A, <b>=A], checked=[<a>, <b>]");
		assertCondition("if <a> is assigned or <b> is not assigned",
				"yes=[], no=[<a>=A, <b>=N], checked=[<a>, <b>]");
		assertCondition("if <a> is not assigned or a.length == 0",
				"yes=[], no=[<a>=N], checked=[<a>]");

		assertCondition("if a is initial or b is initial",
				"yes=[], no=[a=N, b=N], checked=[a, b]");
		assertCondition("if a is initial or b is initial or c is initial",
				"yes=[], no=[a=N, b=N, c=N], checked=[a, b, c]");
		assertCondition("if a is not initial or b is not initial",
				"yes=[], no=[a=A, b=A], checked=[a, b]");
		assertCondition("if a is not initial or b is initial",
				"yes=[], no=[a=A, b=N], checked=[a, b]");
		assertCondition("if a is initial or a.length == 0",
				"yes=[], no=[a=N], checked=[a]");
	}

	/** Tests some conditions that cannot be understood. */
	public void testIllegible() throws IOException {
		assertCondition("if foo and <a> is assigned or bar > 22",
				"yes=[], no=[], checked=[<a>]");
		assertCondition(
				"if <a> is assigned or <b> is not assigned and <c> is not assigned",
				"yes=[], no=[], checked=[<a>, <b>, <c>]");
		assertCondition(
				"if (<a> is assigned and <a>-length > 0) or <a> is not assigned",
				"yes=[], no=[], checked=[<a>]");
		assertCondition("if foo", "yes=[], no=[], checked=[]");

		assertCondition("if foo and a is not initial or bar > 22",
				"yes=[], no=[], checked=[a]");
		assertCondition("if a is not initial or b is initial and c is initial",
				"yes=[], no=[], checked=[a, b, c]");
		assertCondition(
				"if (a is not initial and a-length > 0) or a is initial",
				"yes=[], no=[], checked=[a]");
		assertCondition("if foo", "yes=[], no=[], checked=[]");
	}

	/** Tests other conditional constructs such as "while" etc. */
	public void testOtherConstructs() throws IOException {
		assertCondition("while <a> is not assigned and <b> is not assigned",
				"yes=[<a>=A, <b>=A], no=[], checked=[<a>, <b>]");
		assertCondition("else if <a> is not assigned and <b> is not assigned",
				"yes=[<a>=A, <b>=A], no=[], checked=[<a>, <b>]");
		assertCondition("else if <a> is not assigned",
				"yes=[<a>=A], no=[<a>=N], checked=[<a>]");

		assertCondition("while a is initial and b is initial",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("else if a is initial and b is initial",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("else if a is initial",
				"yes=[a=A], no=[a=N], checked=[a]");
	}

	/** Tests old bugs. */
	public void testRegressions() throws IOException {
		assertCondition("if ( not a is initial ) and ( a->color <> color ).",
				"yes=[a=N], no=[], checked=[a]");
	}

	/**
	 * Asserts that the given code parses to a condition with the given string
	 * representation.
	 */
	private void assertCondition(String code, String expected)
			throws IOException {
		List<IToken> tokens = scan(code, ELanguage.ABAP);
		AbapConditionHeuristic heuristic = new AbapConditionHeuristic(null);
		Condition condition = heuristic.parseCondition(tokens);
		assertEquals("condition(" + expected + ")", condition.toString());
	}

}
