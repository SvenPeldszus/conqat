/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: ITokenPattern.java 51452 2015-01-03 15:23:01Z hummelb $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils.tokens;

import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenPatternMatch;
import org.conqat.engine.sourcecode.dataflow.utils.tokens.TokenStream;

/**
 * A pattern that may be matched against a {@link TokenStream}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 51452 $
 * @ConQAT.Rating GREEN Hash: CBAA1CBC7390B3CA85B582BAD4013697
 */
/* package */interface ITokenPattern {

	/**
	 * Returns true if the pattern matches the given stream and records any
	 * matched groups in the given {@link TokenPatternMatch}.
	 * 
	 * A pattern that does not match must rewind the stream to the position it
	 * was in at the time this method was called.
	 */
	public boolean matches(TokenStream stream, TokenPatternMatch match);

}
