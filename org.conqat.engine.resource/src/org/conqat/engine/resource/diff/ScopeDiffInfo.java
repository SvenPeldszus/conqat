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
package org.conqat.engine.resource.diff;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.pattern.PatternTransformationList;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.text.TextElementUtils;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.TransformedUniformPathToElementMap;
import org.conqat.lib.commons.clone.DeepCloneException;
import org.conqat.lib.commons.clone.IDeepCloneable;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.collections.UnmodifiableSet;

/**
 * This class describes the difference between two scopes. We introduced it to
 * have a proper object that describes the distribution.
 * <p>
 * The term <i>main</i> refers to the primary resource tree (typically the
 * versioning system's trunk), while <i>comparee</i> is the version we compare
 * to (typically some older snapshot).
 * <p>
 * Instances of this class are immutable.
 * 
 * @author $Author: deissenb $
 * @version $Rev: 48647 $
 * @ConQAT.Rating YELLOW Hash: 641E911AB64255C4889B02EE39EBF4FE
 */
public class ScopeDiffInfo implements IDeepCloneable {

	/** Elements in main scope. Maps from uniform path to element. */
	private final Map<String, ITextElement> mainMap;

	/** Elements in comparee scope. */
	private final TransformedUniformPathToElementMap<ITextElement> compareeMap;

	/** Set of added elements (only present in main scope). */
	private final Set<ITextElement> addedElements = new HashSet<ITextElement>();

	/**
	 * Map of removed elements (only present in comparee scope). Maps from
	 * uniform path to element.
	 */
	private final Map<String, ITextElement> removedElements = new HashMap<>();

	/** Set of modified elements (all elements are present in both scopes). */
	private final Set<ITextElement> modifiedElements = new HashSet<ITextElement>();

	/** Set of unmodified elements (all elements are present in both scopes). */
	private final Set<ITextElement> unmodifiedElements = new HashSet<ITextElement>();

	/**
	 * Sum of all line churn, evaluates {@link ScopeDiffer#KEY_CHURN_LINES}.
	 */
	private int churnLines = 0;

	/**
	 * Flag which indicates if line churn information is valid, e. g. for all
	 * elements in the main root line churn information is stored.
	 */
	boolean validLineChurn = true;

	/** Sum of lines in removed elements. */
	private int linesInRemovedElements;

	/**
	 * Sum of normalized lines (base for relative churn)
	 */
	private int normalizedLines;

	/** The input descriptor required for deep cloning. */
	private InputDescriptor inputDescriptor;

	/** Constructor. */
	public ScopeDiffInfo(ITextResource mainRoot, ITextResource compareeRoot,
			PatternTransformationList transformations, IConQATLogger logger)
			throws ConQATException {
		this(new InputDescriptor(mainRoot, compareeRoot, transformations,
				logger));
	}

	/** Constructor. */
	private ScopeDiffInfo(InputDescriptor inputDescriptor)
			throws ConQATException {
		this.inputDescriptor = inputDescriptor;
		mainMap = ResourceTraversalUtils.createUniformPathToElementMap(
				inputDescriptor.mainRoot, ITextElement.class);
		compareeMap = ResourceTraversalUtils
				.createTransformedUniformPathToElementMap(
						inputDescriptor.compareeRoot, ITextElement.class,
						inputDescriptor.transformations, inputDescriptor.logger);

		determineModifiedElements();
	}

	/**
	 * Determine which elements are modified and store both the
	 * {@link #modifiedElements} as well as the {@link #unmodifiedElements}.
	 */
	@SuppressWarnings("unchecked")
	private void determineModifiedElements() throws ConQATException {

		for (String uniformPath : CollectionUtils.unionSet(mainMap.keySet(),
				compareeMap.lookupPaths())) {

			Pair<ITextElement, ITextElement> elements = getElements(uniformPath);

			ITextElement mainElement = elements.getFirst();
			ITextElement compareeElement = elements.getSecond();

			if (mainElement == null) {
				removedElements.put(compareeElement.getUniformPath(),
						compareeElement);
				int compareeLines = TextElementUtils.getNormalizedContent(
						compareeElement).size();
				linesInRemovedElements += compareeLines;
			} else if (compareeElement == null) {
				addedElements.add(mainElement);
				addLineCount(mainElement);
			} else if (isElementContentEqual(mainElement, compareeElement)) {
				unmodifiedElements.add(mainElement);
				addLineCount(mainElement);
			} else {
				modifiedElements.add(mainElement);
				addLineCount(mainElement);
			}
		}
	}

	/**
	 * Adds the count of churn lines and normalized lines of the given element
	 * to this diff info. Requires that the keys
	 * {@value ScopeDiffer#KEY_CHURN_LINES} and
	 * {@value ScopeDiffer#KEY_NORMALIZED_LINES} are set for the given value.
	 */
	private void addLineCount(ITextElement mainElement) {
		try {
			churnLines += NodeUtils.getDoubleValue(mainElement,
					ScopeDiffer.KEY_CHURN_LINES);
			normalizedLines += NodeUtils.getDoubleValue(mainElement,
					ScopeDiffer.KEY_NORMALIZED_LINES);
		} catch (ConQATException e) {
			validLineChurn = false;
			churnLines = -1;
			normalizedLines = -1;
		}
	}

	/** Checks whether the normalized content of the elements equals. */
	private boolean isElementContentEqual(ITextElement mainElement,
			ITextElement compareeElement) throws ConQATException {
		List<String> elementLines = TextElementUtils
				.getNormalizedContent(mainElement);
		List<String> compareeLines = TextElementUtils
				.getNormalizedContent(compareeElement);

		return elementLines.equals(compareeLines);
	}

	/** Get all elements that have been added to the main scope. */
	public UnmodifiableSet<ITextElement> getAddedElements() {
		return CollectionUtils.asUnmodifiable(addedElements);
	}

	/** Get all elements that have been removed from the comparee scope. */
	public Set<ITextElement> getRemovedElements() {
		// we know this is a set
		return new HashSet<>(removedElements.values());
	}

	/**
	 * Get all elements that have been modified but are present in both scopes.
	 * The elements from the main scope are returned.
	 */
	public UnmodifiableSet<ITextElement> getModifiedElements() {
		return CollectionUtils.asUnmodifiable(modifiedElements);
	}

	/**
	 * Get all elements that have <b>not</b> been modified but are present in
	 * both scopes. The elements from the main scope are returned.
	 */
	public UnmodifiableSet<ITextElement> getUnmodifiedElements() {
		return CollectionUtils.asUnmodifiable(unmodifiedElements);
	}

	/** Get a count of all elements present in either main or comparee. */
	public int getTotalElementCount() {
		return addedElements.size() + removedElements.size()
				+ unmodifiedElements.size() + modifiedElements.size();
	}

	/**
	 * Creates a deep copy.
	 */
	@Override
	public ScopeDiffInfo deepClone() throws DeepCloneException {
		try {
			return new ScopeDiffInfo(inputDescriptor.deepClone());
		} catch (ConQATException e) {
			throw new DeepCloneException(e);
		}
	}

	/** Gets the total number of churn lines. */
	public int getChurnLines() {
		return churnLines;
	}

	/**
	 * Determines if a valid line churn information is present, e. g. line churn
	 * information was set at all elements in the scope.
	 */
	public boolean hasValidLineChurn() {
		return validLineChurn;
	}

	/** Gets the number of lines in removed elements. */
	public int getLinesInRemovedElements() {
		return linesInRemovedElements;
	}

	/**
	 * Gets the total normalized line length. That is the base for relative
	 * churn. Does not include the lines in removed elements.
	 */
	public int getNormalizedLines() {
		return normalizedLines;
	}

	/**
	 * Get main an comparee element for a specified uniform path.
	 * 
	 * @param uniformPath
	 *            uniform path from the main scope, uniform pathes from the
	 *            baseline scope can be used to query removed elements.
	 * @return a pair where first stores the element from the main scope and
	 *         second the element from the comparee scope. If the element was
	 *         removed or added, the respective element is null. At least one of
	 *         the pair entries is guaranteed to be non-null.
	 * @throws ConQATException
	 *             if neither a main nor a comparee element could be located or
	 *             the provided uniform path
	 */
	public Pair<ITextElement, ITextElement> getElements(String uniformPath)
			throws ConQATException {
		ITextElement mainElement = mainMap.get(uniformPath);
		ITextElement compareeElement = compareeMap.getElement(uniformPath);

		// we check if a uniform path from the comparee scope was used
		if (compareeElement == null) {
			compareeElement = removedElements.get(uniformPath);
		}

		if (mainElement == null && compareeElement == null) {
			throw new ConQATException("Path " + uniformPath
					+ " could not be found in this "
					+ ScopeDiffInfo.class.getSimpleName());
		}

		return new Pair<>(mainElement, compareeElement);
	}

	/**
	 * This calls {@link #getElements(String)} and retrieves the values stored
	 * at a specific key. If the value was not found or the element is not
	 * present in the main or comparee scope, a default value is returned.
	 * 
	 * @throws ConQATException
	 *             if neither a main nor a comparee element could be located or
	 *             the provided uniform path
	 */
	public <T> Pair<T, T> getValues(String uniformPath, String key,
			Class<T> type, T defaultValue) throws ConQATException {

		Pair<ITextElement, ITextElement> elements = getElements(uniformPath);

		ITextElement mainElement = elements.getFirst();
		ITextElement compareeElement = elements.getSecond();

		T mainValue = defaultValue;
		T compareeValue = defaultValue;

		if (mainElement != null) {
			mainValue = NodeUtils
					.getValue(mainElement, key, type, defaultValue);
		}

		if (compareeElement != null) {
			compareeValue = NodeUtils.getValue(compareeElement, key, type,
					defaultValue);
		}

		return new Pair<>(mainValue, compareeValue);
	}

	/**
	 * This class stores the input data. We put this in a explicit class as we
	 * only need the input data for deep cloning and do not want to clutter the
	 * exterior class with four more fields.
	 */
	private static class InputDescriptor implements IDeepCloneable {
		/** Root of the main scope. */
		private final ITextResource mainRoot;

		/** Root of the comparee scope. */
		private final ITextResource compareeRoot;

		/** Transformations. */
		private final PatternTransformationList transformations;

		/** Logger. */
		private final IConQATLogger logger;

		/** Constructor. */
		private InputDescriptor(ITextResource mainRoot,
				ITextResource compareeRoot,
				PatternTransformationList transformations, IConQATLogger logger) {
			this.mainRoot = mainRoot;
			this.compareeRoot = compareeRoot;
			this.transformations = transformations;
			this.logger = logger;
		}

		/** Create deep copy. The logger of the copy is null. */
		@Override
		public InputDescriptor deepClone() throws DeepCloneException {
			return new InputDescriptor(mainRoot.deepClone(),
					compareeRoot.deepClone(), transformations.deepClone(), null);
		}
	}
}
