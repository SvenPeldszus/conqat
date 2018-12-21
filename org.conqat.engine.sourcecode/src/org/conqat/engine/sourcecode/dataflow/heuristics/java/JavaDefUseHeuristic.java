/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaDefUseHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

import static org.conqat.lib.scanner.ETokenType.AND;
import static org.conqat.lib.scanner.ETokenType.AT_OPERATOR;
import static org.conqat.lib.scanner.ETokenType.BOOLEAN;
import static org.conqat.lib.scanner.ETokenType.BYTE;
import static org.conqat.lib.scanner.ETokenType.CHAR;
import static org.conqat.lib.scanner.ETokenType.COMMA;
import static org.conqat.lib.scanner.ETokenType.DOT;
import static org.conqat.lib.scanner.ETokenType.DOUBLE;
import static org.conqat.lib.scanner.ETokenType.ELLIPSIS;
import static org.conqat.lib.scanner.ETokenType.EXTENDS;
import static org.conqat.lib.scanner.ETokenType.FINAL;
import static org.conqat.lib.scanner.ETokenType.FLOAT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.INT;
import static org.conqat.lib.scanner.ETokenType.LBRACK;
import static org.conqat.lib.scanner.ETokenType.LONG;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.QUESTION;
import static org.conqat.lib.scanner.ETokenType.RBRACK;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SHORT;
import static org.conqat.lib.scanner.ETokenType.SUPER;

import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeDefUseHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.java.JavaConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStreamParser;

/**
 * Uses a heuristic to find identifier definitions and uses.
 * 
 * The lists of operators in this class were compiled using
 * http://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: F254135EE713FFDBF1CD8220FDD138AC
 */
public class JavaDefUseHeuristic extends CLikeDefUseHeuristicBase {

	/** All primitive types of Java. */
	private static final EnumSet<ETokenType> PRIMITIVE_TYPES = EnumSet.of(INT,
			DOUBLE, BOOLEAN, SHORT, CHAR, BYTE, LONG, FLOAT);

	/** Tokens that may appear inside a generics expression. */
	private static final EnumSet<ETokenType> GENERICS_CONTENT = EnumSet.of(
			IDENTIFIER, QUESTION, DOT, COMMA, SUPER, EXTENDS, AND, LBRACK,
			RBRACK);
	static {
		GENERICS_CONTENT.addAll(PRIMITIVE_TYPES);
	}

	/**
	 * Constructor.
	 */
	public JavaDefUseHeuristic(IConQATLogger logger) {
		super(EnumSet.of(FINAL, ELLIPSIS), PRIMITIVE_TYPES, GENERICS_CONTENT,
				JavaConditionHeuristic.DEREFERENCE_OPERATORS,
				JavaConditionHeuristic.NOT_ALLOWED_BEFORE_IDENTIFIER, logger);
	}

	/** {@inheritDoc} */
	@Override
	protected boolean skipAnnotations(TokenStreamParser parser) {
		while (parser.consumeOneOrZeroOf(EnumSet.of(AT_OPERATOR)) != null) {
			// annotation present
			parser.consumeAlternating(EnumSet.of(IDENTIFIER), EnumSet.of(DOT));
			if (!parser.isAnyOf(EnumSet.of(LPAREN))) {
				// Annotation without arguments
				continue;
			}
			if (!parser.skipBalanced(LPAREN, RPAREN,
					EnumSet.complementOf(EnumSet.of(LPAREN, RPAREN)))) {
				return false;
			}
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public VariableReadWriteInfo parseParameterList(List<IToken> tokens,
			String methodName) {
		return super.parseParameterList(tokens, methodName, true);
	}

}
