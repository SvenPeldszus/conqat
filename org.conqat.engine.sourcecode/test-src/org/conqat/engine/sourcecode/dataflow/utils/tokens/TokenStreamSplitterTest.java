/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenStreamSplitterTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the {@link TokenStreamSplitter}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: CBBDC59BCD4B84E2EEB884FDAE2A3CF2
 */
public class TokenStreamSplitterTest extends TokenTestCaseBase {

	/** Tests splitting a simple nested structure. */
	public void testNesting() throws IOException {
		TokenStreamSplitter splitter = new TokenStreamSplitter(
				scan("1 ( 2 ) 3"));
		splitter.splitNested(LPAREN, RPAREN);
		assertSplit(splitter, split("1", "(", "sentinel", ")", "3"), split("2"));
	}

	/** Tests splitting a doubly nested structure. */
	public void testDoubleNesting() throws IOException {
		TokenStreamSplitter splitter = new TokenStreamSplitter(
				scan("(1 ( 2 ) 3)"));
		splitter.splitNested(LPAREN, RPAREN);
		assertSplit(splitter, split("(", "sentinel", ")"),
				split("1", "(", "sentinel", ")", "3"), split("2"));
	}

	/** Tests splitting two different nested structures. */
	public void testDifferentNesting() throws IOException {
		TokenStreamSplitter splitter = new TokenStreamSplitter(
				scan("[1 ( 2 [ 4 ] ) 3]"));
		splitter.splitNested(LPAREN, RPAREN);
		splitter.splitNested(LBRACK, RBRACK);
		assertSplit(splitter, split("[", "sentinel", "]"),
				split("1", "(", "sentinel", ")", "3"),
				split("2", "[", "sentinel", "]"), split("4"));
	}

	/** Tests two nested structures on the same level. */
	public void testNestingTwice() throws IOException {
		TokenStreamSplitter splitter = new TokenStreamSplitter(
				scan("( 1 ) ( 2 )"));
		splitter.splitNested(LPAREN, RPAREN);
		splitter.splitNested(LBRACK, RBRACK);
		assertSplit(splitter,
				split("(", "sentinel", ")", "(", "sentinel", ")"), split("1"),
				split("2"));
	}

	/**
	 * Asserts that the given splitter generates the given expected splits.
	 */
	private void assertSplit(TokenStreamSplitter splitter, String[]... splits) {
		HashSet<List<String>> expectedSet = new HashSet<List<String>>();
		for (String[] split : splits) {
			expectedSet.add(Arrays.asList(split));
		}

		HashSet<List<String>> actualSet = new HashSet<List<String>>();
		for (List<IToken> tokens : splitter.getTokenStreams()) {
			List<String> strings = new ArrayList<String>();
			for (IToken token : tokens) {
				strings.add(token.getText());
			}
			actualSet.add(strings);
		}
		assertEquals(expectedSet, actualSet);
	}

	/**
	 * Shorthand for declaring a split. Makes the tests more readable.
	 */
	private String[] split(String... tokenText) {
		return tokenText;
	}

	/**
	 * Shorthand for generating the tokens from the given Java code. Makes the
	 * tests more readable.
	 */
	private List<IToken> scan(String code) throws IOException {
		return scan(code, ELanguage.JAVA);
	}

}
