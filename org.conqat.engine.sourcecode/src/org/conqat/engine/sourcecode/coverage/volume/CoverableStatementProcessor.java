package org.conqat.engine.sourcecode.coverage.volume;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.shallowparser.SubTypeNames;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.predicate.IPredicate;
import org.conqat.lib.commons.predicate.PredicateUtils;

/**
 * {@ConQAT.Doc}
 *
 * @author $Author: kinnen $
 * @version $Rev: 51015 $
 * @ConQAT.Rating GREEN Hash: A1E3E871FA74BADA8E0D23214BFDAF40
 */
@AConQATProcessor(description = "Reports about coverable statements.")
public class CoverableStatementProcessor extends CoverableEntityProcessorBase {

	/** The subtypes that should be filtered for statement coverage. */
	private static final Set<String> FILTERED_SUBTYPES = new HashSet<>(
			Arrays.asList(SubTypeNames.ELSE, SubTypeNames.TRY,
					SubTypeNames.FINALLY));

	/** Constructor. */
	public CoverableStatementProcessor() {
		super(EShallowEntityType.STATEMENT);
	}

	/** {@inheritDoc} */
	@Override
	protected Collection<ShallowEntity> filterEntities(
			List<ShallowEntity> entities) {
		return filterNonCoverableStatements(entities);
	}

	/**
	 * Filters the given list and returns a list that does no longer contain
	 * unconverable statements which are only there for syntactic reasons, such
	 * as "else".
	 */
	public static Collection<ShallowEntity> filterNonCoverableStatements(
			List<ShallowEntity> entities) {
		return PredicateUtils.obtainNonContained(entities,
				new IPredicate<ShallowEntity>() {
					@Override
					public boolean isContained(ShallowEntity element) {
						return isWhenWithinCase(element)
								|| FILTERED_SUBTYPES.contains(element
										.getSubtype());
					}
				});
	}

	/** Returns whether the entity is a when within a case statement (Ada). */
	private static boolean isWhenWithinCase(ShallowEntity element) {
		return SubTypeNames.WHEN.equalsIgnoreCase(element.getSubtype())
				&& element.getParent() != null
				&& SubTypeNames.CASE.equalsIgnoreCase(element.getParent()
						.getSubtype());
	}

}
