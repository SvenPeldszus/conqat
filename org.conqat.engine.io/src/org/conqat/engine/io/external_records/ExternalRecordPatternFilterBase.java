/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.testgap
 |                                                                       |
   $Id: ExternalRecordPatternFilterBase.java 51617 2015-01-27 14:27:26Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2014 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.io.external_records;

import java.util.EnumMap;
import java.util.Map;
import java.util.Map.Entry;

import org.conqat.engine.commons.filter.FilterBase;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.pattern.IncludeExcludeRegexSupport;
import org.conqat.engine.commons.traversal.ETargetNodes;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.resource.IResource;

/**
 * Filters resource leaf nodes based on patterns on external record fields. The
 * assumption is that each element has at most one external record at a given
 * key, and that elements have to match *all* include rules but no exclude rule
 * to be kept. Elements without an external record are always kept.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51617 $
 * @ConQAT.Rating GREEN Hash: EE3F43348801348041AC33CF9623DC75
 */
public abstract class ExternalRecordPatternFilterBase<E extends Enum<E>>
		extends FilterBase<IResource> {

	/**
	 * Describes the assumption for when an element is filtered. This can be
	 * used in subclasses' processor description.
	 */
	public static final String ASSUMPTION_DOC = "The assumption is that each element "
			+ "has at most one external record at a given key, and that elements have to "
			+ "match *all* include rules but no exclude rule to be kept. Elements without "
			+ "an external record are always kept.";

	/** Just filters leaf nodes, because only these may have external records. */
	@Override
	protected ETargetNodes getTargetNodes() {
		return ETargetNodes.LEAVES;
	}

	/** The Map from field to inclusion patterns. */
	private final Map<E, IncludeExcludeRegexSupport> patterns = new EnumMap<>(
			getEnumType());

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "include", description = "Include regex patterns. If patterns for "
			+ "multiple fields are provided, elements have to be included wrt. to all of them to be kept.")
	public void addInclude(
			@AConQATAttribute(name = "field", description = "External record field") E field,
			@AConQATAttribute(name = "regex", description = "Field value regular expression") String pattern) {
		getOrCreatePatternSupport(field).addIncludePattern(pattern);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "exclude", description = "Exclude regex patterns. Elements must not match "
			+ "any to be kept.")
	public void addExclude(
			@AConQATAttribute(name = "field", description = "External record field") E field,
			@AConQATAttribute(name = "regex", description = "Field value regular expression") String pattern) {
		getOrCreatePatternSupport(field).addExcludePattern(pattern);
	}

	/**
	 * Determines whether an element should be included. Elements are only
	 * included if they are selected by the inclusion / exclusion patterns. All
	 * other elements are always included. An element has to be included with
	 * respect to *all* fields in order to be included.
	 */
	@Override
	protected boolean isFiltered(IResource element) {
		ExternalDataRecord<E> record = NodeUtils.getSingleValue(element,
				getKey(), null);
		if (record == null) {
			return false;
		}

		for (Entry<E, String> entry : record.entrySet()) {
			IncludeExcludeRegexSupport rule = patterns.get(entry.getKey());
			String value = entry.getValue();
			if (rule != null && value != null && !rule.isIncluded(value)) {
				return true;
			}
		}
		return false;
	}

	/** Gets or creates a pattern support rule for the given key. */
	private IncludeExcludeRegexSupport getOrCreatePatternSupport(E key) {
		if (!patterns.containsKey(key)) {
			patterns.put(key, new IncludeExcludeRegexSupport());
		}
		return patterns.get(key);
	}

	/** The enum storing the field names. */
	protected abstract Class<E> getEnumType();

	/** The key storing the field values as an external record. */
	protected abstract String getKey();
}