/*-----------------------------------------------------------------------+
 | eu.cqse.conqat.engine.abap
 |                                                                       |
   $Id: UniqueAbapElementName.java 51460 2015-01-09 10:52:39Z heinemann $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.abap;

import org.conqat.lib.commons.assertion.CCSMPre;

/**
 * Unique name for ABAP source elements, consisting of the name of an ABAP
 * source code object and its type.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 51460 $
 * @ConQAT.Rating RED Hash: 3529A9D35A4F9367C17E84A4D987D1F3
 */
public class UniqueAbapElementName {

	/**
	 * The name of the source code object, must not be null. The name is
	 * expected as within the SAP system, e.g. using '/' for namespace limiters,
	 * not '!' as in the file path.
	 */
	private final String objectName;

	/** The type of the source code object, must not be null. */
	private final EAbapObjectType objectType;

	/** Constructor. */
	public UniqueAbapElementName(String objectName, EAbapObjectType objectType) {
		CCSMPre.isNotNull(objectName);
		CCSMPre.isNotNull(objectType);
		this.objectName = objectName.toUpperCase();
		this.objectType = objectType;
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		return objectType.hashCode() + 31 * objectName.hashCode();
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof UniqueAbapElementName)) {
			return false;
		}
		UniqueAbapElementName other = (UniqueAbapElementName) obj;
		return objectType == other.objectType
				&& objectName.equals(other.objectName);
	}

	/** Returns objectName. */
	public String getObjectName() {
		return objectName;
	}

	/** Returns objectType. */
	public EAbapObjectType getObjectType() {
		return objectType;
	}

	/**
	 * Returns the full unique name of the element, which is essentially a
	 * concatenation of the object name and the object type separated with @.
	 */
	// TODO (LH) Would not encode the return type in the method name, i.e. would
	// call this 'getUniqueName'
	public String getUniqueNameString() {
		return objectName + "@" + objectType;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getUniqueNameString();
	}

}
