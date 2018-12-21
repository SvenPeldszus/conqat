/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaConditionHeuristicTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

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
 * @ConQAT.Rating YELLOW Hash: 9C7CB6372D7AD61186B974A00078E15F
 */
public class JavaConditionHeuristicTest extends TokenTestCaseBase {

	/** Tests instanceof checks. */
	public void testInstanceOf() throws IOException {
		assertCondition("if (a instanceof String) {",
				"yes=[a=N], no=[], checked=[]");
		assertCondition("if (a instanceof String && b instanceof String) {",
				"yes=[a=N, b=N], no=[], checked=[]");
		assertCondition("if (a instanceof String && b == null) {",
				"yes=[a=N, b=A], no=[], checked=[b]");
		assertCondition("if (a instanceof String && b != null) {",
				"yes=[a=N, b=N], no=[], checked=[b]");
		assertCondition("if (a instanceof String || b instanceof String) {",
				"yes=[], no=[], checked=[]");
		assertCondition("if (a instanceof String || b == null) {",
				"yes=[], no=[b=N], checked=[b]");
	}

	/**
	 * Asserts that the given code parses to a condition with the given string
	 * representation.
	 */
	private void assertCondition(String code, String expected)
			throws IOException {
		List<IToken> tokens = scan(code, ELanguage.JAVA);
		JavaConditionHeuristic heuristic = new JavaConditionHeuristic(null);
		Condition condition = heuristic.parseCondition(tokens);
		assertEquals("condition(" + expected + ")", condition.toString());
	}

}
