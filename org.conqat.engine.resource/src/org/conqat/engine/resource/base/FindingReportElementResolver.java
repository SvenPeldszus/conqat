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
package org.conqat.engine.resource.base;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.lib.commons.collections.CounterSet;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.filesystem.CanonicalFile;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Element resolver for {@link FindingCreatorBase} processor.
 * <p>
 * The resolver has to be initialized by calling {@link #init(ITextResource)}
 * with the scope root.
 * 
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 51266 $
 * @ConQAT.Rating YELLOW Hash: 47E95714ED2C4288ABFA87FE32B9A44D
 */
public class FindingReportElementResolver implements IConQATParameterObject {

	/**
	 * The value indicating that the temp directory should be used for the
	 * directory.
	 */
	private final static String TMPDIR_VALUE = "<tmp-dir>";

	/**
	 * Creates a relation between location prefixes (usually directories) and
	 * projects. If this is empty, matching is performed via uniform paths.
	 * 
	 * @see #resolveViaUniformPath()
	 */
	private final PairList<String, String> prefixToProject = new PairList<String, String>();

	/**
	 * If {@link #resolveViaUniformPath()} is true, this maps from uniform path
	 * to element. If false, this maps from location to element.
	 */
	protected Map<String, ITextElement> elementLocationMap;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "case-insensitive", attribute = "value", description = "Defines whether the path names in the report "
			+ "should be treated in a case-insensitive manner.", optional = true)
	public boolean caseInsensitive = false;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "extended-mapping", attribute = "mode", description = "If extended-mapping is used, all path suffixes of each element are added to the element map. Defaults to false, but the default may be overwritten by subclasses. Warning: Enabling this can consume a lot of memory!", optional = true)
	public boolean extendedMappingEnabled;

	/**
	 * PatternList of files that are excluded from finding annotation; default
	 * is the empty list, i.e. all locations are included.
	 */
	protected PatternList excludeLocations = new PatternList();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "exclude-locations", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Sets the list of locations to exclude from finding annotation.")
	public void setExcludeLocations(
			@AConQATAttribute(name = ConQATParamDoc.PATTERN_LIST, description = ConQATParamDoc.PATTERN_LIST_DESC) PatternList patternList)
			throws ConQATException {
		PatternList.checkIfEmpty(patternList);
		excludeLocations = patternList;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "map", minOccurrences = 0, description = "Defines a mapping from a filename prefix "
			+ "(usually a directory) to a project name. This is used to create a uniform path.")
	public void addPrefixToProject(
			@AConQATAttribute(name = "prefix", defaultValue = TMPDIR_VALUE, description = "The directory/prefix") String prefix,
			@AConQATAttribute(name = "project", description = "The project name") String project)
			throws ConQATException {

		if (TMPDIR_VALUE.equals(prefix)) {
			// The name of the temp file (foo..) does not matter, as we are
			// only interested in the parent directory
			CanonicalFile tempFile = parentProcessor.getProcessorInfo()
					.getTempFile("foo", StringUtils.EMPTY_STRING);
			prefix = tempFile.getParentFile().getCanonicalPath();
		}

		// we introduced this after we had a very hard to debug error
		prefix = prefix.trim();
		prefix = UniformPathUtils.normalizeAllSeparators(prefix);

		for (int i = 0; i < prefixToProject.size(); ++i) {
			if (prefix.startsWith(prefixToProject.getFirst(i))
					|| prefixToProject.getFirst(i).startsWith(prefix)) {
				throw new ConQATException(
						"Directory prefixes in map may not be prefixes from each other!");
			}
		}

		prefixToProject.add(prefix, project);
	}

	/** The processor holding this parameter object. */
	private final FindingCreatorBase<?> parentProcessor;

	/** Constructor. */
	public FindingReportElementResolver(FindingCreatorBase<?> parentProcessor) {
		this.parentProcessor = parentProcessor;
		this.extendedMappingEnabled = parentProcessor
				.getExtendetMappingEnabledDefault();
	}

	/** Initializes the element resolver with the given root element. */
	public void init(ITextResource root) throws ConQATException {
		if (resolveViaUniformPath()) {
			elementLocationMap = ResourceTraversalUtils
					.createUniformPathToElementMap(root, ITextElement.class);
		} else {
			elementLocationMap = ResourceTraversalUtils
					.createLocationToElementMap(root, ITextElement.class);
		}

		if (caseInsensitive) {
			elementLocationMap = ResourceTraversalUtils
					.toLowercase(elementLocationMap);
		}

		if (extendedMappingEnabled) {
			elementLocationMap = extendPathMapBySuffixes(elementLocationMap);
		}
	}

	/**
	 * Creates a uniform path from a location. This is performed by checking the
	 * {@link #prefixToProject} table for prefixes.
	 */
	private String resolveUniformPath(String location) {
		location = UniformPathUtils.normalizeAllSeparators(location);
		if (caseInsensitive) {
			location = location.toLowerCase();
		}

		for (int i = 0; i < prefixToProject.size(); ++i) {
			String prefix = prefixToProject.getFirst(i);
			if (caseInsensitive) {
				prefix = prefix.toLowerCase();
			}

			if (location.startsWith(prefix)) {
				location = StringUtils.stripPrefix(location, prefix);
				location = StringUtils.stripPrefix(location,
						UniformPathUtils.SEPARATOR);
				String replacementPrefix = prefixToProject.getSecond(i);
				if (!replacementPrefix.isEmpty()) {
					replacementPrefix += UniformPathUtils.SEPARATOR;
				}
				return replacementPrefix + location;
			}
		}
		return location;
	}

	/** Determines if resolution is performed via uniform paths or locations. */
	protected boolean resolveViaUniformPath() {
		return !prefixToProject.isEmpty();
	}

	/**
	 * Obtains and element for the given location in the report. If the location
	 * is excluded, null is returned. If no element was found, an
	 * {@link UnknownLocationException} is thrown.
	 */
	/* package */ITextElement obtainElement(String locationInReport)
			throws UnknownLocationException {
		String elementLocation = getElementLocation(locationInReport);

		if (excludeLocations.matchesAny(elementLocation)) {
			return null;
		}

		if (caseInsensitive) {
			elementLocation = elementLocation.toLowerCase();
		}

		ITextElement element = getElementFromMap(elementLocation);
		if (element == null) {
			throw new UnknownLocationException();
		}

		return element;
	}

	/**
	 * Checks if the location in the report can be resolved, i.e. is present in
	 * the scope.
	 */
	public boolean isValidLocation(String locationInReport) {
		String elementLocation = getElementLocation(locationInReport);
		return getElementFromMap(elementLocation) != null;
	}

	/**
	 * Checks if the location is excluded.
	 */
	public boolean isExcluded(String locationInReport) {
		String elementLocation = getElementLocation(locationInReport);
		return excludeLocations.matchesAny(elementLocation);
	}

	/**
	 * Converts from a location the report to an element location of the current
	 * scope.
	 */
	private String getElementLocation(String locationInReport) {
		if (resolveViaUniformPath()) {
			return resolveUniformPath(locationInReport);
		}
		return ResourceTraversalUtils.normalizeLocation(locationInReport);
	}

	/**
	 * Returns an element for the given location from the map of stored
	 * elements. If the element does not exist, null is returned.
	 */
	private ITextElement getElementFromMap(String location) {
		if (caseInsensitive) {
			location = location.toLowerCase();
		}

		return elementLocationMap.get(location);
	}

	/**
	 * Add all suffixes of existing paths in the elementMap to the map. Doesn't
	 * add suffixes that are not unique. Newly added suffixes are added using
	 * their normalized location.
	 */
	public static Map<String, ITextElement> extendPathMapBySuffixes(
			Map<String, ITextElement> elementMap) {
		CounterSet<String> pathCounter = new CounterSet<String>();
		Map<String, ITextElement> copyMap = new HashMap<String, ITextElement>(
				elementMap);
		for (Entry<String, ITextElement> entry : copyMap.entrySet()) {
			String path = entry.getKey();
			ITextElement pathElement = entry.getValue();

			path = UniformPathUtils.createSystemIndependentPath(path);

			if (!elementMap.containsKey(path)) {
				elementMap.put(ResourceTraversalUtils.normalizeLocation(path),
						pathElement);
				pathCounter.inc(path);
			}

			// The paths are normalized at this step so we can safely split by
			// forward slash
			while (path.contains(UniformPathUtils.SEPARATOR)) {
				int nextSlash = path.indexOf(UniformPathUtils.SEPARATOR);
				path = path.substring(nextSlash + 1);
				elementMap.put(path, pathElement);
				pathCounter.inc(path);
			}
		}

		return removeUnuniqueEntries(elementMap, pathCounter);
	}

	/**
	 * Removes elements from the elementMap that have a count > 1 in the
	 * pathCounter
	 */
	private static Map<String, ITextElement> removeUnuniqueEntries(
			Map<String, ITextElement> elementMap, CounterSet<String> pathCounter) {
		for (String path : pathCounter.getKeysByValueDescending()) {
			if (pathCounter.getValue(path) > 1) {
				elementMap.remove(path);
			} else {
				// No longer have values > 1
				break;
			}
		}
		return elementMap;
	}

	/** Exception that indicates that a location is unknown. */
	public static class UnknownLocationException extends Exception {
		/** Version used for serialization. */
		private static final long serialVersionUID = 1;
	}
}
