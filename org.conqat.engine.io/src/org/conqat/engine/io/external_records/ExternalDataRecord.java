/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.abap
 |                                                                       |
   $Id: ExternalDataRecord.java 51779 2015-02-13 13:11:38Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.io.external_records;

import java.util.EnumMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.driver.instance.ConQATStringPool;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.clone.IDeepCloneable;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Holds the data of a record of an external data file and makes it accessible
 * by the elements of an enumeration type.
 * 
 * @param <C>
 *            enumeration type of column names
 * 
 * @author $Author: streitel $
 * @version $Rev: 51779 $
 * @ConQAT.Rating GREEN Hash: B625037DA8D0A36746A351A302A4E242
 */
public class ExternalDataRecord<C extends Enum<C>> implements IDeepCloneable {

	/**
	 * Map which holds the values of a data record. NEver <code>null</code>.
	 */
	private final EnumMap<C, String> valuesMap;

	/**
	 * Constructor.
	 * 
	 * @param keyType
	 *            the Class of the field enumeration type
	 * @param fields
	 *            the list of fields where to set the values
	 * @param fieldValues
	 *            the list of values, must be in the same sequence as fields.
	 */
	public ExternalDataRecord(Class<C> keyType, List<C> fields,
			List<String> fieldValues) throws ConQATException {
		if (fields.size() != fieldValues.size()) {
			throw new ConQATException(
					"Number of header cells does not match number of line cells.");
		}
		this.valuesMap = new EnumMap<C, String>(keyType);
		for (int i = 0; i < fields.size(); i++) {
			this.valuesMap.put(fields.get(i),
					ConQATStringPool.intern(fieldValues.get(i)));
		}
	}

	/**
	 * Constructor that directly takes an enum map.
	 */
	/* package */ExternalDataRecord(EnumMap<C, String> values) {
		CCSMPre.isNotNull(values, "Enum map must not be null");
		this.valuesMap = values;
	}

	/** Copy Constructor */
	private ExternalDataRecord(ExternalDataRecord<C> other) {
		this.valuesMap = new EnumMap<C, String>(other.valuesMap);
	}

	/** Gets a value for the given column */
	public String getValue(C column) {
		return valuesMap.get(column);
	}

	/** Returns all enum constants the record has values for. */
	public Set<C> keySet() {
		return valuesMap.keySet();
	}

	/** Returns all mappings stored in the record. */
	public Set<Entry<C, String>> entrySet() {
		return valuesMap.entrySet();
	}

	/** {@inheritDoc} */
	@Override
	public ExternalDataRecord<C> deepClone() {
		return new ExternalDataRecord<C>(this);
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return StringUtils.concat(valuesMap.entrySet(), ";");
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		return valuesMap.hashCode();
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ExternalDataRecord<?>)) {
			return false;
		}

		ExternalDataRecord<?> other = (ExternalDataRecord<?>) obj;
		return valuesMap.equals(other.valuesMap);
	}
}
