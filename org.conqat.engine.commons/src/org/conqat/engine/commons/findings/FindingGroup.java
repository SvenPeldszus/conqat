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
package org.conqat.engine.commons.findings;

import java.util.HashMap;
import java.util.Map;

import org.conqat.engine.commons.findings.location.ElementLocation;
import org.conqat.engine.commons.node.ConQATNodeBase;
import org.conqat.engine.commons.node.IRemovableConQATNode;
import org.conqat.lib.commons.clone.DeepCloneException;

/**
 * A group of findings that collects related findings, such as the clones of a
 * clone group or findings indicating the same flaw.
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47627 $
 * @ConQAT.Rating GREEN Hash: 81D12CCEB56FE44A1BDB070610D54307
 */
public class FindingGroup extends ConQATNodeBase implements
		IRemovableConQATNode {

	/** The category this belongs to. */
	private final FindingCategory category;

	/** The name for this group. */
	private final String name;

	/** The findings in this group. */
	private final Map<Integer, Finding> findings = new HashMap<Integer, Finding>();

	/** Counter for generating globally unique child IDs. */
	private static int idCounter = 0;

	/**
	 * Hidden constructor. Use the factory method in {@link FindingCategory}
	 * instead.
	 */
	/* package */FindingGroup(String name, FindingCategory category) {
		this.name = name;
		this.category = category;
	}

	/** Copy constructor. */
	/* package */FindingGroup(FindingGroup other, FindingCategory category)
			throws DeepCloneException {
		super(other);
		this.category = category;
		name = other.name;
		for (Finding finding : other.findings.values()) {
			findings.put(finding.id, new Finding(finding, this));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void remove() {
		category.remove(this);
	}

	/** Removes a finding. */
	public void remove(Finding finding) {
		findings.remove(finding.id);
		category.getParent().incrementRemoveCounter();
	}

	/** {@inheritDoc} */
	@Override
	public String getId() {
		return category.getId() + ":" + getName();
	}

	/** {@inheritDoc} */
	@Override
	public String getName() {
		return name;
	}

	/** {@inheritDoc} */
	@Override
	public FindingCategory getParent() {
		return category;
	}

	/** {@inheritDoc} */
	@Override
	public boolean hasChildren() {
		return !findings.isEmpty();
	}

	/** {@inheritDoc} */
	@Override
	public Finding[] getChildren() {
		return findings.values().toArray(new Finding[findings.size()]);
	}

	/** Returns a finding by id (or null). */
	/* package */Finding getFindingById(int id) {
		return findings.get(id);
	}

	/** Creates a new finding. */
	public Finding createFinding(ElementLocation location) {
		Finding finding = new Finding(this, ++idCounter, location);
		findings.put(finding.id, finding);
		return finding;
	}

	/** Adds a copy of a finding to the map */
	public void copyFinding(Finding finding) throws DeepCloneException {
		Finding copy = new Finding(finding, this);
		findings.put(copy.id, copy);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Throws an exception as we do not support cloning at this level.
	 */
	@Override
	public IRemovableConQATNode deepClone() {
		throw new UnsupportedOperationException(
				"Deep cloning not supported at this level.");
	}

	/** Returns the number of children/findings. */
	public int getChildrenSize() {
		return findings.size();
	}
}
