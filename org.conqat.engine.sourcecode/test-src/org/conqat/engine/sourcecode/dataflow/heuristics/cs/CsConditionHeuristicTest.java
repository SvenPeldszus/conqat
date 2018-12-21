/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: CsConditionHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.cs;

import java.io.IOException;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.Condition;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the condition parsing heuristic for C#.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: 57ABB9CED901D7A6942B36EF989EE97C
 */
public class CsConditionHeuristicTest extends TokenTestCaseBase {

	/** Tests "is" type checks. */
	public void testIs() throws IOException {
		assertCondition("if (a is String) {", "yes=[a=N], no=[], checked=[]");
		assertCondition("if (a is String && b is String) {",
				"yes=[a=N, b=N], no=[], checked=[]");
		assertCondition("if (a is String && b == null) {",
				"yes=[a=N, b=A], no=[], checked=[b]");
		assertCondition("if (a is String && b != null) {",
				"yes=[a=N, b=N], no=[], checked=[b]");
		assertCondition("if (a is String || b is String) {",
				"yes=[], no=[], checked=[]");
		assertCondition("if (a is String || b == null) {",
				"yes=[], no=[b=N], checked=[b]");
	}

	/**
	 * Asserts that the given code parses to a condition with the given string
	 * representation.
	 */
	private void assertCondition(String code, String expected)
			throws IOException {
		List<IToken> tokens = scan(code, ELanguage.CS);
		CsConditionHeuristic heuristic = new CsConditionHeuristic(null);
		Condition condition = heuristic.parseCondition(tokens);
		assertEquals("condition(" + expected + ")", condition.toString());
	}
}
