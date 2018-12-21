/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: HeuristicBase.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import org.conqat.engine.core.logging.IConQATLogger;

/**
 * Manages a logger for the heuristic.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: FC10EB143A14FAA33A8E1196189DDFE1
 */
public class HeuristicBase {

	/** A logger used for error reporting. */
	protected final IConQATLogger logger;

	/** Sets the logger to use for error reporting. */
	public HeuristicBase(IConQATLogger logger) {
		this.logger = logger;
	}

}
