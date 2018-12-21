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
package org.conqat.engine.dotnet.test;

import org.conqat.engine.commons.node.ConQATNodeBase;
import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.lib.commons.clone.DeepCloneException;

/**
 * Generic free base class for representing a test result hierarchy.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 71E7023191C5936AB6C8276E0A4641BF
 */
// TODO (FS) "generic free" is not necessary IMO. makes little sense outside the
// context of our review
public abstract class TestNodeBase extends ConQATNodeBase implements
		IRemovableConQATNode {

	/**
	 * The name of the node, e.g. the fulld qualified name of a test class.
	 */
	private final String name;

	/** Constructor. */
	public TestNodeBase(String name) {
		this.name = name;
	}

	/** Copy constructor. */
	public TestNodeBase(TestNodeBase node) throws DeepCloneException {
		super(node);
		this.name = node.name;
	}

	/** {@inheritDoc} */
	@Override
	public String getId() {
		return getName();
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return name;
	}

	/** {@inheritDoc} */
	@Override
	public void remove() {
		if (getParent() == null) {
			return; // Cannot remove a root node.
		}

		getParent().removeChild(this);
	}

	/**
	 * Adds a child to this {@link TestNodeContainer}.
	 * <p>
	 * To avoid generic madness this method accepts any kind of
	 * {@link TestNodeContainer} but it has to be ensured that the appropriate
	 * child type has been passed. Otherwise a {@link ClassCastException} is
	 * raised.
	 */
	// TODO (FS) I would expect this in TestNodeContainer, not here
	// TODO (FS) also, the comment does not match the signature
	/* package */abstract void addChild(TestNodeBase child);

	/**
	 * Removes a child from this {@link TestNodeContainer}.
	 * <p>
	 * To avoid generic madness this method accepts any kind of
	 * {@link TestNodeContainer} and removes the passed child based on the node
	 * name.
	 */
	// TODO (FS) I would expect this in TestNodeContainer, not here
	// TODO (FS) also, the comment does not match the signature
	/* package */abstract void removeChild(TestNodeBase child);

	/** {@inheritDoc} */
	@Override
	// TODO (FS) I would expect this to be implemented here. every node has a
	// parent, but not every node is a container with children
	public abstract TestNodeBase getParent();

	/** {@inheritDoc} */
	@Override
	public TestNodeBase deepClone() throws DeepCloneException {
		if (getParent() == null) {
			return deepClone(null);
		}
		return deepClone(getParent().deepClone());
	}

	/** Performs a deep clone using the given parent. */
	// TODO (FS) these methods are only called from this class. make this
	// package visible here and in all subclasses to prevent someone calling
	// this from outside
	public abstract TestNodeBase deepClone(TestNodeBase parent)
			throws DeepCloneException;

}
