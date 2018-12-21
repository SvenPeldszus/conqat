/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 the ConQAT Project                                   |
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
package org.conqat.engine.dotnet.types;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;

/**
 * Base class for code entities
 * 
 * @author $Author: streitel $
 * @version $Rev: 51681 $
 * @ConQAT.Rating YELLOW Hash: 6CB7F6559252F4EED8F6415BF11447F7
 */
public abstract class CodeEntityBase {

	/** Children of the code entity */
	protected final List<NamedCodeEntity> children = new ArrayList<NamedCodeEntity>();

	/** String that separates children */
	private final String childSeparator;

	/** The type of the entity. */
	private final ECodeEntityType type;

	/**
	 * The shallow entity that corresponds to this code entity. May be
	 * <code>null</code> for the {@link RootCodeEntity}.
	 */
	private final ShallowEntity entity;

	/** Constructor */
	protected CodeEntityBase(ECodeEntityType type, ShallowEntity entity,
			String childSeparator) {
		this.type = type;
		this.childSeparator = childSeparator;
		this.entity = entity;
	}

	/** Add child */
	public void addChild(NamedCodeEntity child) {
		children.add(child);
	}

	/** Get children */
	public UnmodifiableList<NamedCodeEntity> getChildren() {
		return CollectionUtils.asUnmodifiable(children);
	}

	/** Get fully qualified name. Can be null, if entity has no name */
	public String getFqName() {
		return null;
	}

	/** @see #entity */
	public ShallowEntity getShallowEntity() {
		return entity;
	}

	/** @see #type */
	public ECodeEntityType getType() {
		return type;
	}

	/** String that separates children */
	public String getChildSeparator() {
		return childSeparator;
	}

	/**
	 * Collects all names of all types and namespaces under this entity in DFS
	 * order.
	 */
	public List<String> collectTypeNames() {
		List<NamedCodeEntity> types = new ArrayList<>();
		collectEntities(this,
				EnumSet.of(ECodeEntityType.NAMESPACE, ECodeEntityType.TYPE),
				types);

		List<String> typeNames = new ArrayList<>();
		for (NamedCodeEntity typeEntity : types) {
			String name = typeEntity.getFqName();
			if (name != null) {
				typeNames.add(name);
			}
		}
		return typeNames;
	}

	/**
	 * Collects all methods under this entity in DFS order.
	 */
	public List<NamedCodeEntity> collectMethods() {
		List<NamedCodeEntity> methods = new ArrayList<>();
		collectEntities(this, EnumSet.of(ECodeEntityType.METHOD), methods);
		return methods;
	}

	/**
	 * Collects all code entities of the given types under the given start
	 * entity in DFS order to the given list
	 */
	private void collectEntities(CodeEntityBase entity,
			EnumSet<ECodeEntityType> collectedTypes,
			List<NamedCodeEntity> result) {
		for (NamedCodeEntity child : entity.getChildren()) {
			if (collectedTypes.contains(child.getType())) {
				result.add(child);
			}
			collectEntities(child, collectedTypes, result);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getFqName();
	}

	/** Types of code entities. */
	public enum ECodeEntityType {

		/** Artificial root entity. Contains all other entities. */
		ROOT,

		/** A namespace. Contains types. */
		NAMESPACE,

		/** A type, e.g. a class or a delegate. Contains methods. */
		TYPE,

		/** A method. */
		METHOD,
	}

}