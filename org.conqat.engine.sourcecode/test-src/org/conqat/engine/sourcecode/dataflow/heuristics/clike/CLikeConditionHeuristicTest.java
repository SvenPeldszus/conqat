/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CLikeConditionHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.clike;

import java.io.IOException;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the condition parsing heuristic for C-like languages.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: BF1D4D1781A6BC6DD7EFC3CDF605DFFD
 */
public class CLikeConditionHeuristicTest extends TokenTestCaseBase {

	/** Tests null checks of a single variable. */
	public void testSingleCondition() throws IOException {
		assertCondition("if (a == null) {", "yes=[a=A], no=[a=N], checked=[a]");
		assertCondition("if (a != null) {", "yes=[a=N], no=[a=A], checked=[a]");
	}

	/** Tests null-checks chained with other things using &&. */
	public void testAndChain() throws IOException {
		assertCondition("if (a == null && b == null) {",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("if (a == null && b == null && c == null) {",
				"yes=[a=A, b=A, c=A], no=[], checked=[a, b, c]");
		assertCondition("if (a != null && b != null) {",
				"yes=[a=N, b=N], no=[], checked=[a, b]");
		assertCondition("if (a != null && b == null) {",
				"yes=[a=N, b=A], no=[], checked=[a, b]");

		assertCondition("if (a != null && a.length > 0) {",
				"yes=[a=N], no=[], checked=[a]");
	}

	/** Tests null-checks chained with other things using ||. */
	public void testOrChain() throws IOException {
		assertCondition("if (a == null || b == null) {",
				"yes=[], no=[a=N, b=N], checked=[a, b]");
		assertCondition("if (a == null || b == null || c == null) {",
				"yes=[], no=[a=N, b=N, c=N], checked=[a, b, c]");
		assertCondition("if (a != null || b != null) {",
				"yes=[], no=[a=A, b=A], checked=[a, b]");
		assertCondition("if (a != null || b == null) {",
				"yes=[], no=[a=A, b=N], checked=[a, b]");

		assertCondition("if (a == null || a.length == 0) {",
				"yes=[], no=[a=N], checked=[a]");
	}

	/** Tests some conditions that cannot be understood. */
	public void testIllegible() throws IOException {
		assertCondition("if (foo() && a != null || b.c == null) {",
				"yes=[], no=[], checked=[a]");
		assertCondition("if (a != null || b == null && c == null) {",
				"yes=[], no=[], checked=[a, b, c]");
		assertCondition("if ((a != null && a.length > 0) || a == null) {",
				"yes=[], no=[], checked=[a]");
		assertCondition("if (foo()) {", "yes=[], no=[], checked=[]");
	}

	/** Tests other conditional constructs such as "do..while" etc. */
	public void testOtherConstructs() throws IOException {
		assertCondition("while (a == null && b == null) {",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("} while (a == null && b == null);",
				"yes=[a=A, b=A], no=[], checked=[a, b]");
		assertCondition("else if (a == null && b == null) {",
				"yes=[a=A, b=A], no=[], checked=[a, b]");

		assertCondition("} while (a == null);",
				"yes=[a=A], no=[a=N], checked=[a]");
		assertCondition("else if (a == null) {",
				"yes=[a=A], no=[a=N], checked=[a]");
	}

	/**
	 * Asserts that the given code parses to a condition with the given string
	 * representation.
	 */
	private void assertCondition(String code, String expected)
			throws IOException {
		List<IToken> tokens = scan(code, ELanguage.JAVA);
		CLikeConditionHeuristic heuristic = new CLikeConditionHeuristic(null,
				EnumSet.of(ETokenType.DOT, ETokenType.LBRACK),
				EnumSet.of(ETokenType.DOT));
		Condition condition = heuristic.parseCondition(tokens);
		assertEquals("condition(" + expected + ")", condition.toString());
	}

}
