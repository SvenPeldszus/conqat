/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ExitStatementEntityMatcher.java 51148 2014-11-14 13:51:19Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;

import java.util.List;

import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPattern;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * Matches application exit statements in object-oriented languages, e.g.
 * "System.exit" or "Environment.Exit".
 * 
 * @author $Author: streitel $
 * @version $Rev: 51148 $
 * @ConQAT.Rating YELLOW Hash: 7A7884B568DC0D4BD9F433C1A590D518
 */
public class ExitStatementEntityMatcher implements IShallowEntityMatcher {

	/**  */
	private static final TokenPattern EXIT_STATEMENT_PATTERN = new TokenPattern()
			.beginningOfStream().sequence(IDENTIFIER).group(0)
			.optional(DOT, IDENTIFIER).group(0).sequence(ETokenType.LPAREN);

	/** The signature of the exit method. */
	private final String methodSignature;

	/**
	 * Constructor.
	 * 
	 * @param methodSignature
	 *            the signature of the method that exits the program, e.g.
	 *            "System.exit".
	 */
	public ExitStatementEntityMatcher(String methodSignature) {
		this.methodSignature = methodSignature;
	}

	/** {@inheritDoc} */
	@Override
	public boolean matches(ShallowEntity entity) {
		return entity.getSubtype().equals(SubTypeNames.SIMPLE_STATEMENT)
				&& isSystemExitStatement(entity.ownStartTokens());
	}

	/**
	 * Returns <code>true</code> if the given tokens belong to a System.exit()
	 * statement.
	 */
	private boolean isSystemExitStatement(List<IToken> tokens) {
		TokenPatternMatch match = EXIT_STATEMENT_PATTERN.matchFirst(tokens);
		return match != null && match.groupString(0).equals(methodSignature);
	}
}