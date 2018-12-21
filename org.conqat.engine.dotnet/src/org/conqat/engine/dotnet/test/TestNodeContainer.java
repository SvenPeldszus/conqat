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

import java.util.HashMap;
import java.util.Map;

import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.clone.DeepCloneException;
import org.conqat.lib.commons.collections.CollectionUtils;

/**
 * Base class for representing a test result hierarchy with explicit types for
 * the parent and child nodes.
 * 
 * @param <PARENT>
 *            The type of the node parent.
 * @param <CHILD>
 *            The type of the child nodes.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 1441EA756D359F21227C1D9829F14E86
 */
// TODO (FS) I like the new base class. I would expect a different separation of
// duties between it and this class, though: the base class should care about
// the parent, this class about the children. this way, TestMethod could
// implement only the base class (as a method is not really a container)
// see also my comments in TestNodeBase
public abstract class TestNodeContainer<PARENT extends TestNodeBase, CHILD extends TestNodeBase>
		extends TestNodeBase implements IRemovableConQATNode {

	/** The parent test node. */
	private final PARENT parent;

	/**
	 * Map of test node children. The key of the map is the ID of the stored
	 * node.
	 */
	private final Map<String, CHILD> children = new HashMap<>();

	/** The class object of the child elements. */
	private final Class<CHILD> childClass;

	/** Constructor. */
	protected TestNodeContainer(PARENT parent, String name,
			Class<CHILD> childClass) {
		super(name);
		this.parent = parent;
		this.childClass = childClass;

		if (parent != null) {
			parent.addChild(this);
		}
	}

	/**
	 * Copy constructor.
	 * 
	 * @param parent
	 *            The new (already cloned) parent of the node that is about to
	 *            be cloned.
	 * @param node
	 *            The node that is about to be cloned.
	 */
	protected TestNodeContainer(PARENT parent,
			TestNodeContainer<PARENT, CHILD> node) throws DeepCloneException {
		super(node);

		if (parent != null) {
			CCSMAssert
					.isFalse(
							parent == node.parent,
							"The copy constructor expects the parent node to be already cloned (= different)"
									+ "from the parent of the node that is about to be cloned.");
		}

		this.parent = parent;
		this.childClass = node.childClass;

		for (CHILD child : node.children.values()) {
			addChild(child.deepClone(this));
		}
	}

	/** {@inheritDoc} */
	@Override
	public PARENT getParent() {
		return parent;
	}

	/** {@inheritDoc} */
	@Override
	/* package */void addChild(TestNodeBase child) {
		children.put(child.getName(), CCSMAssert.checkedCast(child, childClass));
	}

	/** {@inheritDoc} */
	@Override
	/* package */void removeChild(TestNodeBase child) {
		children.remove(child.getName());
	}

	/**
	 * Gets the child node with the given name, <code>null</code> if no such run
	 * exists.
	 */
	// TODO (FS) comment references "run"
	public CHILD getChild(String name) {
		return children.get(name);
	}

	/** {@inheritDoc} */
	@Override
	public boolean hasChildren() {
		return !children.isEmpty();
	}

	/** {@inheritDoc} */
	@Override
	public CHILD[] getChildren() {
		return CollectionUtils.toArray(children.values(), childClass);
	}
}
