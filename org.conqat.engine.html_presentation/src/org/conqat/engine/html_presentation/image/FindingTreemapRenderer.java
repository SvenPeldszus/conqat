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
package org.conqat.engine.html_presentation.image;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.findings.DetachedFindingsList;
import org.conqat.engine.commons.findings.Finding;
import org.conqat.engine.commons.findings.FindingCategory;
import org.conqat.engine.commons.findings.FindingGroup;
import org.conqat.engine.commons.findings.FindingReport;
import org.conqat.engine.commons.findings.FindingsList;
import org.conqat.engine.commons.format.Summary;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.traversal.TraversalUtils;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.html_presentation.color.ColorizerBase;
import org.conqat.engine.html_presentation.treemap.TreeMapCreator;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.Pair;

/**
 * {ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 51595 $
 * @ConQAT.Rating GREEN Hash: E3217A4F85FDD4915BCC1381735FDB63
 */
@AConQATProcessor(description = "This processor renders multiple treemaps to a "
		+ "single page, each of which highlights which nodes contain findings "
		+ "from a particular group. This processors pontentially creates several dozens or "
		+ "even hundreds of treemaps. To prevent memory issues, it does not actually deep clone "
		+ "the input hierarchy but creates a light-weighted copy of the tree that contains only the necessary "
		+ "attributes. Additional keys to copy, e.g. links, must be specified explicitly. One needs to be aware "
		+ "that one treemap is generated for each finding group, the image map, however, that provides the tooltips "
		+ "is the same for all groups.")
public class FindingTreemapRenderer extends HTMLImageRendererBase {

	/** Keys to copy. */
	private final LinkedHashSet<String> keys = new LinkedHashSet<String>();

	/** {ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "input", attribute = "ref", description = "The input scope that should be shown in the treemap and whose nodes are colored according to the contained findings.")
	public IConQATNode input;

	/** {ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "finding-category", attribute = "name", optional = true, description = "Only finding groups from the category with this name are used for rendering treemaps. Default is to use finding groups in all categories.")
	public String findingCategoryName = null;

	/** {ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "finding-group", attribute = "name", optional = true, description = "Only finding groups with this name are used for rendering treemaps. Default is to use all finding groups.")
	public String findingGroupName = null;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "size", attribute = ConQATParamDoc.READKEY_KEY_NAME, optional = true, description = ""
			+ "Set the key used to retrieve the size of a node. "
			+ "If no key is given, each node will be weighted with 1, i.e. just the number of leaves is counted.")
	public String sizeKey = null;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = ConQATParamDoc.READKEY_NAME, minOccurrences = 0, description = ""
			+ "Additional keys to copy (size key is always copied")
	public void setReadKey(
			@AConQATAttribute(name = ConQATParamDoc.READKEY_KEY_NAME, description = ConQATParamDoc.READKEY_KEY_DESC) String key) {
		keys.add(key);
	}

	/** The number of finding groups used to draw treemaps. */
	private int relevantGroups = 0;

	/** {@inheritDoc} */
	@Override
	protected String getIconName() {
		return "tree_map.gif";
	}

	/** {@inheritDoc} */
	@Override
	protected void layoutPage() throws ConQATException {

		if (sizeKey != null) {
			keys.add(sizeKey);
		}

		List<FindingGroup> groups = getRelevantFindingGroups();

		Collections.sort(groups, new Comparator<FindingGroup>() {
			@Override
			public int compare(FindingGroup a, FindingGroup b) {
				return a.getName().compareTo(b.getName());
			}

		});

		for (FindingGroup group : groups) {
			addImageDescriptorForFindingGroup(group);
		}

		super.layoutPage();
	}

	/**
	 * Gets the finding groups that should be used for drawing the treemap
	 * considering the category and group name that has possibly been specified
	 * by the user.
	 */
	private List<FindingGroup> getRelevantFindingGroups()
			throws ConQATException {
		List<FindingGroup> groups = new ArrayList<FindingGroup>();
		FindingReport report = NodeUtils.getFindingReport(input);

		for (FindingCategory category : report.getChildren()) {
			if (findingCategoryName == null
					|| category.getName().equals(findingCategoryName)) {
				addRelevantGroups(category, groups);
			}
		}

		if (groups.isEmpty()) {
			throw new ConQATException(
					"No finding groups found! Category name: "
							+ findingCategoryName + " Group anme: "
							+ findingGroupName);
		}
		return groups;
	}

	/** All all relevant groups from this category to the provided list. */
	private void addRelevantGroups(FindingCategory category,
			List<FindingGroup> groups) {
		for (FindingGroup group : category.getChildren()) {
			if (findingGroupName == null
					|| group.getName().equals(findingGroupName)) {
				groups.add(group);
			}
		}
	}

	/**
	 * Creates the treemap image descriptors that highlights all elements that
	 * contain at least one finding from the given finding group.
	 */
	private void addImageDescriptorForFindingGroup(FindingGroup group)
			throws ConQATException {

		IConQATNode preprocessedInput = NodeUtils.copyTree(input, keys);
		NodeUtils.addToDisplayList(preprocessedInput, sizeKey);

		Pair<Set<String>, Integer> result = prepareInput(preprocessedInput,
				group);
		Set<String> relevantKeys = result.getFirst();
		if (!relevantKeys.isEmpty()) {
			NodeUtils.addToDisplayList(preprocessedInput, relevantKeys);
			relevantGroups++;

			TreeMapCreator creator = new TreeMapCreator();
			creator.init(getProcessorInfo());
			if (sizeKey != null) {
				creator.sizeKey = sizeKey;
			}
			creator.root = preprocessedInput;
			creator.setCushions(TreeMapCreator.DEFAULT_CUSHION_HEIGHT,
					TreeMapCreator.DEFAULT_CUSHION_SCALE);

			super.addImageDescriptor(creator.process(), group.getName() + ":"
					+ result.getSecond());
		}
	}

	/** {@inheritDoc} */
	@Override
	protected Object getSummary() {
		return new Summary(relevantGroups);
	}

	/**
	 * Sets the color for all leaves of the given input tree. By default, the
	 * color is white. If there is a finding attached to the leaf that is part
	 * of the given finding group, the leaf is colored in red. Returns the
	 * following pair: keys for which relevant findings where found, number of
	 * leaves carrying findings from this group.
	 */
	private Pair<Set<String>, Integer> prepareInput(IConQATNode node,
			FindingGroup group) {
		Map<String, IConQATNode> map = TraversalUtils
				.createIdToLeafNodeMap(node);

		Set<String> relevantKeys = new LinkedHashSet<>();
		int affectedLeaves = 0;
		for (IConQATNode leaf : TraversalUtils.listLeavesDepthFirst(input)) {

			IConQATNode newLeaf = map.get(leaf.getId());
			CCSMAssert.isNotNull(newLeaf);

			newLeaf.setValue(ColorizerBase.COLOR_KEY_DEFAULT, Color.WHITE);

			for (String key : NodeUtils.getDisplayList(input).getKeyList()) {

				FindingsList findings = NodeUtils.getFindingsList(leaf, key);

				if (findings == null) {
					continue;
				}

				// The maintainer may wonder why we copy all findings and not
				// only findings from relevant finding groups. The reason is
				// that the base class handles multiple graphical tree maps but
				// only one set of tool tips, defined by the first descriptor
				// added via addImageDescriptor. If we copy only the relevant
				// findings, the findings display are somewhat arbitrarily
				// defined by the first finding group. Copying everything
				// ensures that all the information is there. By reducing the
				// display list to relevant keys (see below) we prevent the
				// treemaps from showing garbage no one wants to see.
				DetachedFindingsList detachedFindingsList = new DetachedFindingsList();
				detachedFindingsList.addAllFindings(findings);
				newLeaf.setValue(key, detachedFindingsList);

				if (hasFindingFromGroup(findings, group)) {
					newLeaf.setValue(ColorizerBase.COLOR_KEY_DEFAULT, Color.RED);
					relevantKeys.add(key);
					affectedLeaves++;
				}

			}
		}
		return new Pair<>(relevantKeys, affectedLeaves);
	}

	/**
	 * Checks if the findings list contains at least one finding from the
	 * provided findings list.
	 */
	private static boolean hasFindingFromGroup(FindingsList findings,
			FindingGroup group) {
		for (Finding finding : findings) {
			if (finding.getParent() == group) {
				return true;
			}
		}
		return false;
	}
}
