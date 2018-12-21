/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
|                                                                          |
| Licensed under the Apache License, Version 2.0 (the "License");          |
| you may not use this file except in compliance with the License.         |
| You may obtain a copy of the License at                                  |
|                                                                          |
|    http://www.apache.org/licenses/LICENSE-2.0                            |
|                                                                          |
| Unless required by applicable law or agreed to in writing, software      |
| distributed under the License is distributed on an "AS IS" BASIS,        |
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.dotnet.ila;

import java.util.List;

/**
 * Value object that stores IL code member information. This class is immutable.
 * 
 * @author juergens
 * @author $Author: streitel $
 * @version $Rev: 50996 $
 * @ConQAT.Rating RED Hash: D7CA08C444EDAC00D4B57186CBA86F74
 */
public class Member {

	/** Member name */
	private final String name;

	/** Member raw name */
	private final String rawName;

	/** Member type */
	private final EMemberType type;

	/** Member visibility */
	private final String visibility;

	/** Abstract modifier of member */
	// TODO (FS) this comment is a bit cryptic. please rephrase
	private final boolean isAbstract;

	/** Member metadata token */
	private final int token;

	/** The method's parent's metadata token. */
	private final int parentToken;

	/** Number of IL statements of the member */
	private final int numberIlStatements;

	/** IL statements of the method body */
	private final String ilStatementSequence;

	/** Flag that determines whether member is synthetic */
	private final boolean isSynthetic;

	/** Method return type. */
	private final String returnType;

	/** Method parameter types. */
	private final List<String> parameterTypes;

	/** Member short name without generics etc. */
	private final String shortName;

	/** Generic parameter string. */
	private final String genericParameters;

	/** Inner class name, if any. */
	// TODO (FS) please document if this is null or the empty string if no inner
	// class name is provided
	private final String innerClassName;

	/** Constructor */
	public Member(String name, String rawName, String shortName,
			EMemberType type, String visibility, boolean isAbstract, int token,
			int numberIlStatements, String ilStatementSequence,
			boolean synthetic, String returnType, List<String> parameterTypes,
			String genericParameters, String innerClassName, int parentToken) {
		this.name = name;
		this.rawName = rawName;
		this.type = type;
		this.visibility = visibility;
		this.isAbstract = isAbstract;
		this.token = token;
		this.numberIlStatements = numberIlStatements;
		this.ilStatementSequence = ilStatementSequence;
		this.isSynthetic = synthetic;
		this.shortName = shortName;
		this.parameterTypes = parameterTypes;
		this.returnType = returnType;
		this.genericParameters = genericParameters;
		this.innerClassName = innerClassName;
		this.parentToken = parentToken;
	}

	/** Returns true, if member is a method. */
	public boolean isMethod() {
		return type.equals(EMemberType.Method);
	}

	/** Returns true, if member is a constructor. */
	public boolean isConstructor() {
		return EMemberType.Constructor == type;
	}

	/**
	 * Returns true, iff member is callable, i.e. either a method or a
	 * constructor.
	 */
	public boolean isCallable() {
		return isMethod() || isConstructor();
	}

	/** Returns the name. */
	// TODO (FS) please use @see #fieldName for the getters and setters
	public String getName() {
		return name;
	}

	/** Returns the raw name. */
	public String getRawName() {
		return rawName;
	}

	/** Returns the short name. */
	public String getShortName() {
		return shortName;
	}

	/** Returns the return type. */
	public String getReturnType() {
		return returnType;
	}

	/** Returns the parameter types. */
	public List<String> getParameterTypes() {
		return parameterTypes;
	}

	/** Returns the type. */
	public EMemberType getType() {
		return type;
	}

	/** Returns the visibility. */
	public String getVisibility() {
		return visibility;
	}

	/** Returns whether this is an abstract member. */
	public boolean isAbstract() {
		return isAbstract;
	}

	/** Returns the metadata token. */
	public int getToken() {
		return token;
	}

	/** Returns the parent type's metadata token. */
	public int getParentToken() {
		return parentToken;
	}

	/** Returns the number of IL statements. */
	public int getNumberIlStatements() {
		return numberIlStatements;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return name;
	}

	/** Returns the IL statements of the method body. */
	public String getILStatementSequence() {
		return ilStatementSequence;
	}

	/** Returns whether this is a synthetic member. */
	public boolean isSynthetic() {
		return isSynthetic;
	}

	/** Returns the generic parameter string. */
	public String getGenericParameters() {
		return genericParameters;
	}

	/** Whether this member is part of an inner class. */
	public boolean isPartOfInnerClass() {
		return innerClassName != null;
	}

	/** Returns the inner class name. */
	public String getInnerClassName() {
		return innerClassName;
	}
}