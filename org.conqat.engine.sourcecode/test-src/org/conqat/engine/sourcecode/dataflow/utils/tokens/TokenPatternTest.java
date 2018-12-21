/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: TokenPatternTest.java 51565 2015-01-20 13:00:50Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.DO;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.IF;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.LTEQ;
import static org.conqat.lib.scanner.ETokenType.NULL_LITERAL;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.WHILE;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Tests the {@link TokenPattern}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: FCF6C440DBB52896B81C9748C8F2061B
 */
public class TokenPatternTest extends TokenTestCaseBase {

	/** Tests a simple sequence of tokens. */
	public void testSequence() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(LPAREN, LPAREN).group(0),
				"if ((foo(())))");
		assertEquals(2, matches.size());
		assertMatch(matches, 0, 0, "(", "(");
		assertMatch(matches, 1, 0, "(", "(");
	}

	/** Tests a simple optional pattern. */
	public void testOptional() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().optional(LPAREN).group(0), "if ((");
		assertEquals(3, matches.size());
		assertMatch(matches, 0, 0);
		assertMatch(matches, 1, 0, "(");
		assertMatch(matches, 2, 0, "(");
	}

	/** Tests a simple repeated pattern. */
	public void testRepeated() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().repeated(LPAREN).group(0), "if(((");
		assertEquals(4, matches.size());
		assertMatch(matches, 0, 0);
		assertMatch(matches, 1, 0, "(", "(", "(");
		assertMatch(matches, 2, 0, "(", "(");
		assertMatch(matches, 3, 0, "(");
	}

	/** Tests a simple skip-to pattern. */
	public void testSkipTo() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(LPAREN)
						.skipTo(IDENTIFIER, NULL_LITERAL).group(0),
				"if((a(null(");
		assertEquals(3, matches.size());
		assertMatch(matches, 0, 0, "a");
		assertMatch(matches, 1, 0, "a");
		assertMatch(matches, 2, 0, "null");
	}

	/** Tests a simple "not followed by" pattern. */
	public void testNotFollowedBy() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(LPAREN).group(0)
						.notFollowedBy(LPAREN), "if(((x(");
		assertEquals(2, matches.size());
		assertMatch(matches, 0, 0, "(");
		assertMatch(matches, 1, 0, "(");

		List<TokenPatternMatch> matches2 = match(
				new TokenPattern().sequence(IF).notFollowedBy(LPAREN)
						.repeated(IDENTIFIER).group(0), "if a b");
		assertEquals(1, matches2.size());
		assertMatch(matches2, 0, 0, "a", "b");
	}

	/** Tests a simple "not preceded by" pattern. */
	public void testNotPrecededBy() throws IOException {
		List<TokenPatternMatch> matches = match(new TokenPattern()
				.notPrecededBy(LPAREN).sequence(LPAREN).group(0), "(uiae(((");
		assertEquals(2, matches.size());
		assertMatch(matches, 0, 0, "(");
		assertMatch(matches, 1, 0, "(");
	}

	/** Tests a simple "end of stream" pattern. */
	public void testEndOfStream() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(LPAREN).endOfStream(), "(uiae(((");
		assertEquals(1, matches.size());
	}

	/** Tests a simple "beginning of stream" pattern. */
	public void testBeginningOfStream() throws IOException {
		List<TokenPatternMatch> matches = match(new TokenPattern()
				.beginningOfStream().sequence(LPAREN), "(((uiae(((");
		assertEquals(1, matches.size());
	}

	/** Tests a simple "not at beginning of stream" pattern. */
	public void testNotAtBeginning() throws IOException {
		List<TokenPatternMatch> matches = match(new TokenPattern()
				.notAtBeginningOfStream().sequence(LPAREN), "(((uiae(((");
		assertEquals(5, matches.size());
	}

	/** Tests a simple alternative pattern. */
	public void testAlternative() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().alternative(IDENTIFIER,
						new TokenPattern().sequence(LPAREN, LPAREN, LPAREN))
						.group(0), "(((uiae(((");
		assertEquals(3, matches.size());
		assertMatch(matches, 0, 0, "(", "(", "(");
		assertMatch(matches, 1, 0, "uiae");
		assertMatch(matches, 2, 0, "(", "(", "(");
	}

	/** Tests a simple nested pattern. */
	public void testNested() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(IDENTIFIER)
						.skipNested(LPAREN, RPAREN, false).group(0),
				"uiae(u(i)a)");
		assertEquals(2, matches.size());
		assertMatch(matches, 0, 0, "(", "u", "(", "i", ")", "a", ")");
		assertMatch(matches, 1, 0, "(", "i", ")");

		List<TokenPatternMatch> matches2 = match(new TokenPattern()
				.sequence(LT).skipNested(LPAREN, RPAREN, true).group(0),
				"<<((u(i)a)");
		assertEquals(2, matches2.size());
		assertMatch(matches2, 0, 0);
		assertMatch(matches2, 1, 0);
	}

	/** Tests a complex pattern. */
	public void testComplex() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern().sequence(IF, LPAREN).repeated(LPAREN)
						.sequence(IDENTIFIER).group(0)
						.optional(DOT, IDENTIFIER).group(0).notFollowedBy(LTEQ),
				"if (((a == 2))) {} if (x.y == 3) {} if ( if r == 3 if ((t. == 3 if ((v <= 2 if (y.y <= 2");
		assertEquals(3, matches.size());
		assertMatch(matches, 0, 0, "a");
		assertMatch(matches, 1, 0, "x", ".", "y");
		assertMatch(matches, 2, 0, "t");

		List<TokenPatternMatch> matches2 = match(
				new TokenPattern()
						.beginningOfStream()
						.sequence(IF)
						.optional(
								new TokenPattern().skipTo(IF).sequence(LPAREN)
										.sequence(IDENTIFIER).group(0))
						.optional(
								new TokenPattern()
										.skipTo(WHILE)
										.sequence(LPAREN)
										.repeated(
												new TokenPattern()
														.optional(
																new TokenPattern()
																		.sequence(
																				IDENTIFIER)
																		.sequence(
																				DO))
														.sequence(IDENTIFIER)
														.group(0))),
				"if a if ( a while ( b do c d e do f");
		assertEquals(1, matches2.size());
		assertMatch(matches2, 0, 0, "a", "c", "d", "f");
	}

	/** Tests a sub-pattern. */
	public void testSubPattern() throws IOException {
		List<TokenPatternMatch> matches = match(
				new TokenPattern()
						.sequence(LPAREN)
						.sequence(IDENTIFIER)
						.group(0)
						.repeated(
								new TokenPattern().sequence(COMMA).group(1)
										.sequence(IDENTIFIER).group(0)),
				"(a) (a, b) (a, b , c, d) (a,)");
		assertEquals(4, matches.size());
		assertMatch(matches, 0, 0, "a");
		assertMatch(matches, 0, 1);
		assertMatch(matches, 1, 0, "a", "b");
		assertMatch(matches, 1, 1, ",");
		assertMatch(matches, 2, 0, "a", "b", "c", "d");
		assertMatch(matches, 2, 1, ",", ",", ",");
		assertMatch(matches, 3, 0, "a");
		assertMatch(matches, 3, 1);

		List<TokenPatternMatch> matches2 = match(
				new TokenPattern()
						.sequence(IF)
						.sequence(
								new TokenPattern().optional(LPAREN)
										.sequence(IDENTIFIER).optional(RPAREN))
						.group(0), "if (b)");
		assertEquals(1, matches2.size());
		assertMatch(matches2, 0, 0, "(", "b", ")");

		List<TokenPatternMatch> matches3 = match(
				new TokenPattern().sequence(IF, LPAREN)
						.repeated(new TokenPattern().sequence(IDENTIFIER))
						.sequence(RPAREN).group(0), "if ( if )");
		assertEquals(0, matches3.size());
	}

	/** Asserts that the given list of matches contains the given group. */
	private void assertMatch(List<TokenPatternMatch> matches, int matchIndex,
			int groupIndex, String... expectedGroup) {
		assertTrue(matches.size() > matchIndex);
		assertEquals(Arrays.asList(expectedGroup), matches.get(matchIndex)
				.groupTexts(groupIndex));
	}

	/**
	 * Matches the given pattern against the given java code.
	 */
	private List<TokenPatternMatch> match(TokenPattern pattern, String code)
			throws IOException {
		List<IToken> tokens = scan(code, ELanguage.JAVA);
		List<TokenPatternMatch> matches = pattern.match(tokens);
		return matches;
	}
}
