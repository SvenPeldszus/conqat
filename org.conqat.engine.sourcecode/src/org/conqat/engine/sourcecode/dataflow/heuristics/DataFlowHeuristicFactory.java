/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: DataFlowHeuristicFactory.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics;

import java.util.EnumSet;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.engine.sourcecode.dataflow.heuristics.ConditionHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.IDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.AbapConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.AbapDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.abap.AbapDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.CsConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.CsDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.cs.CsDefUseHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.java.JavaConditionHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.java.JavaDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.java.JavaDefUseHeuristic;

/**
 * Factory for language specific data flow heuristics.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: BDE720733ED395140301CAB2A6F18937
 */
public class DataFlowHeuristicFactory {

	/** The languages that are supported by the data flow framework. */
	public static final EnumSet<ELanguage> SUPPORTED_LANGUAGES = EnumSet.of(
			ELanguage.JAVA, ELanguage.ABAP, ELanguage.CS);

	/**
	 * Returns a new data flow heuristic for the given language.
	 * 
	 * @throws ConQATException
	 *             if the language is not (yet) supported by our framework.
	 */
	public static IDataFlowHeuristic createDataFlowHeuristic(ELanguage language)
			throws ConQATException {
		switch (language) {
		case JAVA:
			return new JavaDataFlowHeuristic();
		case ABAP:
			return new AbapDataFlowHeuristic();
		case CS:
			return new CsDataFlowHeuristic();
		default:
			throw new ConQATException("Data flow analysis for language "
					+ language + " not yet supported!");
		}
	}

	/**
	 * Returns a new def-use heuristic for the given language.
	 * 
	 * @throws ConQATException
	 *             if the language is not (yet) supported by our framework.
	 */
	public static IDefUseHeuristic createDefUseHeuristic(ELanguage language,
			IConQATLogger logger) throws ConQATException {
		switch (language) {
		case JAVA:
			return new JavaDefUseHeuristic(logger);
		case ABAP:
			return new AbapDefUseHeuristic();
		case CS:
			return new CsDefUseHeuristic(logger);
		default:
			throw new ConQATException("Data flow analysis for language "
					+ language + " not yet supported!");
		}
	}

	/**
	 * Returns a new condition heuristic for the given language.
	 * 
	 * @throws ConQATException
	 *             if the language is not (yet) supported by our framework.
	 */
	public static ConditionHeuristicBase createConditionHeuristic(
			ELanguage language, IDefUseHeuristic defUseHeuristic)
			throws ConQATException {
		switch (language) {
		case JAVA:
			return new JavaConditionHeuristic(defUseHeuristic);
		case ABAP:
			return new AbapConditionHeuristic(defUseHeuristic);
		case CS:
			return new CsConditionHeuristic(defUseHeuristic);
		default:
			throw new ConQATException("Data flow analysis for language "
					+ language + " not yet supported!");
		}
	}

	/**
	 * Returns <code>true</code> if the given language is supported by the data
	 * flow framework.
	 */
	public static boolean supportsLanguage(ELanguage language) {
		return SUPPORTED_LANGUAGES.contains(language);
	}

}
