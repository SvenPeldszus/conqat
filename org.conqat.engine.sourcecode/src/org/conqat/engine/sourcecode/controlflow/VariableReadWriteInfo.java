/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: VariableReadWriteInfo.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.sourcecode.controlflow.VariableWrite;

/**
 * Stores information about variable reads and writes that is necessary for all
 * data flow algorithms.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 24133DD3DFAB5D1D946202725006FCD2
 */
public class VariableReadWriteInfo {

	/** The variable assignments that happen in the node. */
	private final List<VariableWrite> assignments = new ArrayList<VariableWrite>();

	/** The variable definitions that happen in the node. */
	private final List<VariableWrite> definitions = new ArrayList<VariableWrite>();

	/**
	 * All variables that are read in the node.
	 */
	private final Set<String> reads = new HashSet<String>();

	/**
	 * All variables that are dereferenced in the node.
	 */
	private final Set<String> dereferences = new HashSet<String>();

	/**
	 * Returns all writes to variables, i.e. definitions and assignments.
	 * Definitions are ensured to occur before assignments in the returned list.
	 */
	public List<VariableWrite> getAllWrites() {
		List<VariableWrite> infos = new ArrayList<VariableWrite>();
		infos.addAll(getAssignments());
		infos.addAll(getDefinitions());
		return infos;
	}

	/** Returns all assignments that happen in the node (without definitions). */
	public List<VariableWrite> getAssignments() {
		return assignments;
	}

	/** Returns all definitions that happen in the node. */
	public List<VariableWrite> getDefinitions() {
		return definitions;
	}

	/** Returns all variables that are read in the node. */
	public Set<String> getReads() {
		return reads;
	}

	/** Returns all variables that are dereferenced in the node. */
	public Set<String> getDereferences() {
		return dereferences;
	}

}
