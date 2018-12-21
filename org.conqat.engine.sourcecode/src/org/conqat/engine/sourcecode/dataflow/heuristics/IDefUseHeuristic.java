/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: IDefUseHeuristic.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.List;

import org.conqat.engine.sourcecode.controlflow.VariableReadWriteInfo;
import org.conqat.lib.scanner.IToken;

/**
 * Interface for def-use heuristics.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: EFC21115A7699EEFF13C898DBAE26C5D
 */
public interface IDefUseHeuristic {

	/** Adds the given variable to the current scope. */
	public void addToScope(String variableName);

	/**
	 * Opens a new variable scope.
	 */
	public void openNewScope();

	/**
	 * Closes the current variable scope.
	 */
	public void closeCurrentScope();

	/**
	 * Returns <code>true</code> if the given variable has been defined.
	 */
	public boolean isKnown(String variable);

	/**
	 * Creates the def-use information that belongs to the node with the given
	 * tokens.
	 */
	public VariableReadWriteInfo parseStatement(List<IToken> tokens);

	/**
	 * Creates the def-use information that belongs to the parameter list with
	 * the given tokens of the method with the given name.
	 */
	public VariableReadWriteInfo parseParameterList(List<IToken> tokens,
			String methodName);

}
