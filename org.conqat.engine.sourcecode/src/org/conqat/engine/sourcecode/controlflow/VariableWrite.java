/*-----------------------------------------------------------------------+
 | org.conqat.engine.index.incubator
 |                                                                       |
   $Id: VariableWrite.java 51147 2014-11-14 10:14:06Z streitel $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.sourcecode.controlflow;

import org.conqat.lib.commons.assertion.CCSMAssert;

/**
 * Stores information about a single write to a variable. The exact semantics of
 * the write are not stored, only which variable is written and whether or not
 * it was assigned exactly one value or variable or an empty definition.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51147 $
 * @ConQAT.Rating YELLOW Hash: 8744118AA34D33BE7F804CDD5DBB10A7
 */
public class VariableWrite {

	/**
	 * The value used to mark assignments of caught exception objects, which can
	 * never be null.
	 */
	public static final String EXCEPTION_VALUE = "exception";

	/**
	 * The value used to mark assignments of newly created (i.e. non-null)
	 * objects.
	 */
	public static final String NEW_OBJECT_VALUE = "new";

	/** The type of value written to the variable. */
	private EVariabeWriteType type = EVariabeWriteType.OTHER;

	/**
	 * If this flag is <code>true</code>, the value assigned to the variable
	 * stems from a default initialization and the dead store analysis should
	 * not consider overwriting this value without a use as bad.
	 */
	private boolean isDefaultInitialization = false;

	/**
	 * The identifier on the left side of the assignment, i.e. the variable
	 * being written.
	 */
	private final String changedVariable;

	/**
	 * The constant value assigned to the variable or <code>null</code> if the
	 * assignment was not a single constant.
	 */
	private String assignedValue;

	/**
	 * The identifier whose value was assigned to the variable or
	 * <code>null</code> if the assignment was not a single variable.
	 */
	private String assignedVariable;

	/** Constructor. The default type is {@link EVariabeWriteType#OTHER}. */
	public VariableWrite(String changedVariable) {
		this.changedVariable = changedVariable;
	}

	/** Returns type of value assigned to the variable. */
	public EVariabeWriteType getType() {
		return type;
	}

	/** Returns changedVariable. */
	public String getChangedVariable() {
		return changedVariable;
	}

	/** Returns assignedValue. */
	public String getAssignedValue() {
		return assignedValue;
	}

	/** Returns assignedVariable. */
	public String getAssignedVariable() {
		return assignedVariable;
	}

	/**
	 * Sets the assigned variable and changes the type of the write to
	 * "variable".
	 */
	public VariableWrite setVariable(String variable) {
		CCSMAssert.isNotNull(variable);
		reset();
		assignedVariable = variable;
		type = EVariabeWriteType.VARIABLE;
		return this;
	}

	/** Sets the assigned value and changes the type of the write to "value". */
	public VariableWrite setValue(String value) {
		CCSMAssert.isNotNull(value);
		reset();
		assignedValue = value;
		type = EVariabeWriteType.VALUE;
		return this;
	}

	/**
	 * Sets the type of the write to "empty". Empty assignments are marked as
	 * default initializations.
	 */
	public VariableWrite setEmpty() {
		reset();
		type = EVariabeWriteType.EMPTY;
		isDefaultInitialization = true;
		return this;
	}

	/** Sets the type of the write to "other". */
	public VariableWrite setOther() {
		reset();
		type = EVariabeWriteType.OTHER;
		return this;
	}

	/** Sets the type of the write to "null". */
	public VariableWrite setNull() {
		reset();
		type = EVariabeWriteType.NULL;
		return this;
	}

	/** Marks this write as a default initialization with a value. */
	public VariableWrite makeDefaultInitialization() {
		isDefaultInitialization = true;
		return this;
	}

	/**
	 * Marks this write as not a default initialization.
	 */
	public VariableWrite removeDefaultInitialization() {
		isDefaultInitialization = false;
		return this;
	}

	/** Returns isDefaultInitialization. */
	public boolean isDefaultInitialization() {
		return isDefaultInitialization;
	}

	/** Resets all stored right side values. */
	private void reset() {
		isDefaultInitialization = false;
		assignedValue = null;
		assignedVariable = null;
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((assignedValue == null) ? 0 : assignedValue.hashCode());
		result = prime
				* result
				+ ((assignedVariable == null) ? 0 : assignedVariable.hashCode());
		result = prime * result
				+ ((changedVariable == null) ? 0 : changedVariable.hashCode());
		result = prime * result + (isDefaultInitialization ? 1231 : 1237);
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		VariableWrite other = (VariableWrite) obj;
		if (assignedValue == null) {
			if (other.assignedValue != null) {
				return false;
			}
		} else if (!assignedValue.equals(other.assignedValue)) {
			return false;
		}
		if (assignedVariable == null) {
			if (other.assignedVariable != null) {
				return false;
			}
		} else if (!assignedVariable.equals(other.assignedVariable)) {
			return false;
		}
		if (changedVariable == null) {
			if (other.changedVariable != null) {
				return false;
			}
		} else if (!changedVariable.equals(other.changedVariable)) {
			return false;
		}
		if (isDefaultInitialization != other.isDefaultInitialization) {
			return false;
		}
		if (type != other.type) {
			return false;
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		String representation = changedVariable + " = ";
		switch (type) {
		case EMPTY:
			representation += "empty";
			break;
		case OTHER:
			representation += "?";
			break;
		case VALUE:
			representation += "value( " + assignedValue.toString() + " )";
			break;
		case VARIABLE:
			representation += "var( " + assignedVariable.toString() + " )";
			break;
		case NULL:
			representation += "null";
			break;
		default:
			CCSMAssert.fail("You need to implement toString for type "
					+ type.name());
		}
		if (isDefaultInitialization) {
			representation += " [default init]";
		}
		return representation;
	}

	/** The type of value written to the variable. */
	public static enum EVariabeWriteType {

		/** An empty definition. */
		EMPTY,

		/** A single variable. */
		VARIABLE,

		/** A single value literal. */
		VALUE,

		/** A null value. */
		NULL,

		/** Anything else, e.g. a complex expression. */
		OTHER

	}
}
