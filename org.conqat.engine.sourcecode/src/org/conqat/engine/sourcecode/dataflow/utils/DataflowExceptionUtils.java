/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DataflowExceptionUtils.java 51535 2015-01-19 07:52:55Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.utils;

import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.TokenStreamUtils;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.scanner.IToken;

/**
 * Utility functions for reporting exceptions.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51535 $
 * @ConQAT.Rating YELLOW Hash: B9032B1101D3E55355CCFDF5BAE239B0
 */
public class DataflowExceptionUtils {

	/**
	 * Creates a message that includes location information based on the given
	 * entity.
	 */
	public static String createMessage(String message, ShallowEntity entity) {
		return createMessage(message, entity.ownStartTokens());
	}

	/**
	 * Creates a message that includes location information based on the given
	 * tokens.
	 */
	public static String createMessage(String message, List<IToken> tokens) {
		if (tokens.isEmpty()) {
			return message + "\nOccurred at an unknown location";
		}
		return createMessage(
				message + "\nTokens: " + TokenStreamUtils.toString(tokens),
				tokens.get(0), CollectionUtils.getLast(tokens));
	}

	/**
	 * Creates a message that includes location information based on the given
	 * tokens.
	 */
	public static String createMessage(String message, IToken startToken,
			IToken endToken) {
		return message + "\nOccurred in " + startToken.getOriginId() + ":"
				+ startToken.getLineNumber() + "-" + endToken.getLineNumber();
	}

}
