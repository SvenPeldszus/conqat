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
package org.conqat.engine.dotnet.ila.xml;

import java.util.Set;

import org.conqat.engine.commons.node.ListNode;
import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.commons.util.ConQATXMLReader;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Base class for classes that read XML files produced by the Intermediate
 * Language Analyzer.
 * 
 * @param <X>
 *            Type of exception that gets thrown.
 * 
 * @author $Author: streitel $
 * @version $Revision: 49975 $
 * @ConQAT.Rating GREEN Hash: DEE4D78ACFC841FAE46473CCC8FF74E8
 */
public abstract class IlaXmlReaderBase<X extends Exception> extends
		ConQATXMLReader<EIlaXmlElement, EIlaXmlAttribute, X> {

	/** Root node under which all encountered class nodes are stored */
	protected final ListNode root;

	/** All members that match any of these patterns are excluded. */
	private final PatternList excludePatterns;

	/**
	 * If not null, only members matching one of these patterns are included
	 */
	private final PatternList includePatterns;

	/** Set of all dependencies that have been imported. */
	private final Set<String> includedDependencies;

	/** Set of all dependencies that have been excluded. */
	private final Set<String> excludedDependencies;

	/** The location string that identifies the currently parsed element. */
	private final String location;

	/**
	 * Constructor.
	 * 
	 * @param element
	 *            XML that gets parsed.
	 * @param root
	 *            Root under which the nodes representing types found during
	 *            parse are attached.
	 * @param ignorePatterns
	 *            List of patterns for members that are ignored during import.
	 * @param includePatterns
	 *            If not null, only members matching one of these patterns are
	 *            included.
	 * @param includedDependencies
	 *            Set into which included dependencies are written.
	 * @param excludedDependencies
	 *            Set into which excluded dependencies are written.
	 */
	public IlaXmlReaderBase(ITextElement element, ListNode root,
			PatternList ignorePatterns, PatternList includePatterns,
			Set<String> includedDependencies, Set<String> excludedDependencies)
			throws ConQATException {
		super(element.getContent(), EIlaXmlAttribute.class, element
				.getEncoding());
		this.root = root;
		this.excludePatterns = ignorePatterns;
		this.includePatterns = includePatterns;
		this.excludedDependencies = excludedDependencies;
		this.includedDependencies = includedDependencies;
		this.location = element.getLocation();
	}

	/** Parses the IL-XML. */
	public void parse() throws ConQATException {
		parseAndWrapExceptions();
		doParse();
	}

	/** Template method that deriving classes override to implement parsing */
	protected abstract void doParse() throws ConQATException;

	/** {@inheritDoc} */
	@Override
	protected String getLocation() {
		return location;
	}

	/**
	 * Adds a dependency to the {@link #includedDependencies} if it is not
	 * excluded by patterns. Otherwise adds the dependency to the
	 * {@link #excludedDependencies}. Returns whether dependency was included.
	 */
	protected boolean processDependency(String source, String target) {
		String dependency = source + " -> " + target;

		if (!StringUtils.isEmpty(target) && isIncluded(target)
				&& !isIgnored(target)) {
			includedDependencies.add(dependency);
			return true;
		}

		excludedDependencies.add(dependency);
		return false;
	}

	/** Checks whether dependency is ignored */
	private boolean isIgnored(String dependency) {
		return excludePatterns != null
				&& excludePatterns.matchesAny(dependency);
	}

	/** Checks whether dependency is included */
	private boolean isIncluded(String dependency) {
		return includePatterns == null
				|| includePatterns.matchesAny(dependency);
	}
}