/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: JavaDataFlowHeuristic.java 51706 2015-02-08 14:25:04Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.dataflow.heuristics.java;

import static org.conqat.engine.sourcecode.shallowparser.SubTypeNames.ANNOTATION;
import static org.conqat.engine.sourcecode.shallowparser.SubTypeNames.SYNCHRONIZED;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.sourcecode.controlflow.ControlFlowNode;
import org.conqat.engine.sourcecode.dataflow.heuristics.DataflowHeuristicBase;
import org.conqat.engine.sourcecode.dataflow.heuristics.ExitStatementEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.IShallowEntityMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.ShallowEntityTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.SubTypeMatcher;
import org.conqat.engine.sourcecode.dataflow.heuristics.clike.CLikeDataFlowHeuristic;
import org.conqat.engine.sourcecode.dataflow.heuristics.java.rules.MethodRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.AnonymousBlockRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ForRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.IControlFlowRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.ReturnRule;
import org.conqat.engine.sourcecode.dataflow.heuristics.rules.TryCatchFinallyRule;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;

/**
 * Data flow heuristics for the Java language.
 * 
 * What this heuristic cannot do:
 * <ul>
 * <li><code>break</code> to label is not supported but could be implemented
 * easily.
 * <li>Exception handling control flow is not accurately represented. See
 * {@link TryCatchFinallyRule}.
 * </ul>
 * 
 * @author $Author: streitel $
 * @version $Rev: 51706 $
 * @ConQAT.Rating YELLOW Hash: C57B94B0EF781DC7F8609816DFB33024
 */
public class JavaDataFlowHeuristic extends DataflowHeuristicBase {

	/** Matches the java.lang.Override annotation */
	public static final IShallowEntityMatcher OVERRIDE_ANNOTATION_MATCHER = new AnnotationEntityMatcher(
			"Override", "java.lang.Override");

	/** Matches the java.lang.SuppressWarnings annotation */
	public static final IShallowEntityMatcher SUPPRESS_WARNINGS_ANNOTATION_MATCHER = new AnnotationEntityMatcher(
			"SuppressWarnings", "java.lang.SuppressWarnings");

	/** Matches the annotations. */
	public static final IShallowEntityMatcher ANNOTATION_MATCHER = new AnnotationEntityMatcher();

	/** Matches annotations with a given name. */
	public static class AnnotationEntityMatcher implements
			IShallowEntityMatcher {

		/** The names of the matched annotations. */
		private final List<String> names;

		/**
		 * Constructor.
		 * 
		 * @param names
		 *            the names of the matched annotations. If no names are
		 *            given, this matches all annotations.
		 */
		public AnnotationEntityMatcher(String... names) {
			this.names = Arrays.asList(names);
		}

		/** {@inheritDoc} */
		@Override
		public boolean matches(ShallowEntity entity) {
			if (entity.getType() == EShallowEntityType.META
					&& entity.getSubtype().equals(ANNOTATION)) {
				return names.isEmpty() || names.contains(entity.getName());
			}
			return false;
		}

	}

	/**
	 * All rules used to transform {@link ShallowEntity}s to
	 * {@link ControlFlowNode}s.
	 */
	private static final PairList<IShallowEntityMatcher, IControlFlowRule> RULES = new PairList<IShallowEntityMatcher, IControlFlowRule>();
	static {
		RULES.add(new ExitStatementEntityMatcher("System.exit"),
				new ReturnRule());
		RULES.add(new SubTypeMatcher(SYNCHRONIZED), new AnonymousBlockRule());
		RULES.add(CLikeDataFlowHeuristic.FOR, new ForRule(ETokenType.COLON));
		RULES.add(ShallowEntityTypeMatcher.METHOD_MATCHER, new MethodRule());
		CLikeDataFlowHeuristic.addDefaultRules(RULES);
	}

	/** Constructor. */
	public JavaDataFlowHeuristic() {
		super(RULES, ELanguage.JAVA);
	}

	/** {@inheritDoc} */
	@Override
	public PairList<String, List<ShallowEntity>> extractExecutables(
			List<ShallowEntity> entities) {
		PairList<String, List<ShallowEntity>> executables = new PairList<String, List<ShallowEntity>>();
		recursivelyAddAllExecutables(entities, executables);
		return executables;
	}

	/**
	 * Recursively adds all entities in the given entity tree to the pair list,
	 * which represent executable code.
	 */
	private void recursivelyAddAllExecutables(List<ShallowEntity> entities,
			PairList<String, List<ShallowEntity>> executables) {
		for (int i = 0; i < entities.size(); i++) {
			ShallowEntity entity = entities.get(i);
			if (ShallowEntityTypeMatcher.METHOD_MATCHER.matches(entity)) {
				if (isValidMethodEntity(entity)) {
					List<ShallowEntity> executableEntities = new ArrayList<ShallowEntity>();
					addMethodAnnotations(entities, i, executableEntities);
					executableEntities.add(entity);
					executables.add(entity.getName(), executableEntities);
				}
			}

			if (!entity.getChildren().isEmpty()) {
				recursivelyAddAllExecutables(entity.getChildren(), executables);
			}
		}
	}

	/**
	 * Adds all annotations that belong to the method that starts at the given
	 * index.
	 */
	private void addMethodAnnotations(List<ShallowEntity> entities, int i,
			List<ShallowEntity> executableEntities) {
		for (int k = i - 1; k >= 0; k--) {
			ShallowEntity annotationEntity = entities.get(k);
			if (ANNOTATION_MATCHER.matches(annotationEntity)) {
				executableEntities.add(0, annotationEntity);
			} else {
				break;
			}
		}
	}

	/**
	 * Returns <code>true</code> if the given entity is a non-abstract,
	 * non-interface method.
	 */
	private boolean isValidMethodEntity(ShallowEntity entity) {
		return !entity.getSubtype().contains("abstract");
	}

}
