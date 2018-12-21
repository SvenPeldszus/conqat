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
package org.conqat.engine.commons.findings.filter;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.conqat.engine.commons.CommonUtils;
import org.conqat.engine.commons.findings.Finding;
import org.conqat.engine.commons.findings.util.FindingGroupPredicate;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.filesystem.AntPatternUtils;

/**
 * {ConQAT.Doc}
 * 
 * @author $Author: deissenb $
 * @version $Rev: 48821 $
 * @ConQAT.Rating YELLOW Hash: 8232370351FD4BF7701F806D5CD1531F
 */
@AConQATProcessor(description = "Filters findings based on regular expressions "
		+ "that match uniform path, finding category, finding group and finding message. "
		+ "A finding is filtered out if all conditions apply. Uniform path patterns "
		+ "can alternatively be described with an Ant expression.")
public class LocationAndMessageFindingsFilter extends FindingsFilterBase {

	/** List of filter descriptors. */
	private List<FilterDescriptor> descriptors = new ArrayList<>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "finding-regex-filter", description = "Patterns for filtering findings.")
	public void setLocationRegexPattern(
			@AConQATAttribute(name = "uniform-path-regex", description = "The uniform path regex pattern.", defaultValue = ".*") String uniformPathRegex,
			@AConQATAttribute(name = "category-regex", description = "The finding category regex pattern.", defaultValue = ".*") String categoryRegex,
			@AConQATAttribute(name = "group-regex", description = "The finding group regex pattern.", defaultValue = ".*") String groupRegex,
			@AConQATAttribute(name = "message-regex", description = "The message regex pattern.", defaultValue = ".*") String messageRegex)
			throws ConQATException {
		descriptors.add(new FilterDescriptor(uniformPathRegex, categoryRegex,
				groupRegex, messageRegex));
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "finding-ant-pattern-filter", description = "Patterns for filtering findings.")
	public void setLocationAntPattern(
			@AConQATAttribute(name = "uniform-path-ant-pattern", description = "The uniform path ant pattern.", defaultValue = "**") String uniformPathAntEx,
			@AConQATAttribute(name = "message-regex", description = "The message regex pattern.", defaultValue = ".*") String messageRegex,
			@AConQATAttribute(name = "category-regex", description = "The finding category regex pattern.", defaultValue = ".*") String categoryRegex,
			@AConQATAttribute(name = "group-regex", description = "The finding group regex pattern.", defaultValue = ".*") String groupRegex,
			@AConQATAttribute(name = "case-sensitive", description = "Flag for disabling case-sensitivity for the path ant pattern.", defaultValue = "true") boolean caseSensitive)
			throws ConQATException {

		descriptors.add(new FilterDescriptor(uniformPathAntEx, caseSensitive,
				categoryRegex, groupRegex, messageRegex));
	}

	/** {@inheritDoc} */
	@Override
	protected boolean isFiltered(IConQATNode node, Finding finding) {
		for (FilterDescriptor descriptor : descriptors) {
			if (descriptor.matches(finding)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Class that describes filter rules.
	 * 
	 * Note: This is somewhat redundant to {@link FindingGroupPredicate} that
	 * also knows about categories and groups. But as this class is agnostic of
	 * the uniform path and itself maintains a list of patterns, I don't think
	 * it is necessary to harmonize them.
	 */
	private static class FilterDescriptor {

		/** Pattern for uniform path. */
		private final Pattern uniformPathPattern;

		/** Pattern for category. */
		private final Pattern categoryPattern;

		/** Pattern for group. */
		private final Pattern groupPattern;

		/** Pattern for message. */
		private final Pattern messagePattern;

		/** Constructor for regexes. */
		public FilterDescriptor(String uniformPathRegex, String categoryRegex,
				String groupRegex, String messageRegex) throws ConQATException {
			this(CommonUtils.compilePattern(uniformPathRegex), categoryRegex,
					groupRegex, messageRegex);
		}

		/** Constructor for regexes and Ant pattern for uniform path. */
		public FilterDescriptor(String uniformPathAntEx, boolean caseSensitive,
				String categoryRegex, String groupRegex, String messageRegex)
				throws ConQATException {
			this(AntPatternUtils
					.convertPattern(uniformPathAntEx, caseSensitive),
					categoryRegex, groupRegex, messageRegex);
		}

		/** Constructor. */
		public FilterDescriptor(Pattern uniformPathPattern,
				String categoryRegex, String groupRegex, String messageRegex)
				throws ConQATException {
			this.uniformPathPattern = uniformPathPattern;
			categoryPattern = CommonUtils.compilePattern(categoryRegex);
			groupPattern = CommonUtils.compilePattern(groupRegex);
			messagePattern = CommonUtils.compilePattern(messageRegex);
		}

		/** Checks if a finding is matched by this descriptor. */
		public boolean matches(Finding finding) {
			return uniformPathMatches(finding) && categoryMatches(finding)
					&& groupMatches(finding) && messageMatches(finding);
		}

		/**
		 * Checks whether the finding's location's uniform path matches the this
		 * descriptor.
		 */
		private boolean uniformPathMatches(Finding finding) {
			return uniformPathPattern.matcher(
					finding.getLocation().getUniformPath()).matches();
		}

		/** Checks if the finding's category matches this descriptor. */
		private boolean categoryMatches(Finding finding) {
			return categoryPattern.matcher(
					finding.getParent().getParent().getName()).matches();
		}

		/** Checks if the finding's group matches this descriptor. */
		private boolean groupMatches(Finding finding) {
			return groupPattern.matcher(finding.getParent().getName())
					.matches();
		}

		/** Checks whether the finding's message matches this descriptor.. */
		private boolean messageMatches(Finding finding) {
			return messagePattern.matcher(finding.getMessage()).matches();
		}

	}

}
