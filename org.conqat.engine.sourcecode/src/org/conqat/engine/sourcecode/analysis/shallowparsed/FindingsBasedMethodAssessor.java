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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.findings.Finding;
import org.conqat.engine.commons.findings.FindingsList;
import org.conqat.engine.commons.findings.location.TextRegionLocation;
import org.conqat.engine.commons.findings.util.FindingUtils;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.analysis.SLOCAnalyzer;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementProcessorBase;
import org.conqat.engine.sourcecode.resource.TokenElementUtils;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.commons.assessment.Assessment;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.region.Region;
import org.conqat.lib.commons.region.RegionSet;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 50919 $
 * @ConQAT.Rating GREEN Hash: 1A2C6CFA78E2F5670A6BE056EFB5FEF6
 */
@AConQATProcessor(description = "Annotates each element with an assessment that reflects the method assessment. "
		+ "Each method is weighted by its size (LOC or SLOC) and the assessment color for the method is determined by "
		+ "overlapping findings, i.e. each method is assigned the color of the worst finding that overlaps with the method. "
		+ "Non region findings (e.g. file-based findings) are ignored for this.")
public class FindingsBasedMethodAssessor extends TokenElementProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = ConQATParamDoc.WRITEKEY_NAME, attribute = ConQATParamDoc.WRITEKEY_KEY_NAME, optional = false, description = ConQATParamDoc.WRITEKEY_DESC)
	public String writeKey;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "finding-default", attribute = "color", optional = true, description = ""
			+ "The color to be used for findings without an explicit assessment. Default is red.")
	public ETrafficLightColor findingDefaultColor = ETrafficLightColor.RED;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "sloc-based", attribute = "value", optional = true, description = ""
			+ "If this is true, methods are weighted by SLOC instead of LOC. Default is false.")
	public boolean slocBased = false;

	/**
	 * The files for which an invalid index warning has been issued. Used to
	 * avoid duplicate warnings for the same file.
	 */
	private final Set<String> invalidFilesWarned = new HashSet<String>();

	/** The keys to look in for findings. */
	private final Set<String> findingsKeys = new HashSet<String>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "findings", description = "If keys are given, all findings stored at these keys are respected. "
			+ "Otherwise, all keys in the display list are respected.")
	public void addFindingsKey(
			@AConQATAttribute(name = "key", description = "The key to look into.") String key) {
		findingsKeys.add(key);
	}

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITokenResource root) {
		if (findingsKeys.isEmpty()) {
			findingsKeys.addAll(NodeUtils.getDisplayList(root).getKeyList());
		}

		NodeUtils.addToDisplayList(root, writeKey);
	}

	/** {@inheritDoc} */
	@Override
	protected void processElement(ITokenElement element) throws ConQATException {
		Map<ETrafficLightColor, RegionSet> findingColorRegions = determineFindingColorRegions(element);

		Assessment assessment = new Assessment();
		for (ShallowEntity method : ShallowEntityTraversalUtils
				.listMethodsNonRecursive(ShallowParserFactory.parse(element,
						getLogger()))) {
			updateAssessmentForMethod(assessment, method, findingColorRegions,
					element);
		}
		element.setValue(writeKey, assessment);
	}

	/**
	 * Calculates a map containing for each color the regions covered by
	 * findings of the corresponding color. The regions correspond to filtered
	 * offsets.
	 */
	private Map<ETrafficLightColor, RegionSet> determineFindingColorRegions(
			ITokenElement element) throws ConQATException {
		Map<ETrafficLightColor, RegionSet> findingColorRegions = new EnumMap<>(
				ETrafficLightColor.class);
		for (String key : findingsKeys) {
			FindingsList findingsList = NodeUtils.getFindingsList(element, key);
			if (findingsList == null) {
				continue;
			}

			for (Finding finding : findingsList) {
				if (!(finding.getLocation() instanceof TextRegionLocation)) {
					continue;
				}
				TextRegionLocation location = (TextRegionLocation) finding
						.getLocation();

				ETrafficLightColor color = FindingUtils.getFindingColor(
						finding, findingDefaultColor);
				RegionSet regions = findingColorRegions.get(color);
				if (regions == null) {
					regions = new RegionSet();
					findingColorRegions.put(color, regions);
				}
				regions.add(new Region(element.getFilteredOffset(location
						.getRawStartOffset()), element
						.getFilteredOffset(location.getRawEndOffset())));
			}
		}
		return findingColorRegions;
	}

	/**
	 * Updates the given assessment by increasing is depending on the size of
	 * the method and the color determined by intersecting its region with the
	 * regions containing findings. This also respects methods within methods,
	 * i.e. all code in "sub" methods is not counted as part of this method.
	 */
	private void updateAssessmentForMethod(Assessment assessment,
			ShallowEntity method,
			Map<ETrafficLightColor, RegionSet> findingColorRegions,
			ITokenElement element) {

		// method without children is not relevant for assessment
		List<ShallowEntity> methodChildren = method.getChildren();
		if (methodChildren.isEmpty()) {
			return;
		}

		List<ShallowEntity> childMethods = ShallowEntityTraversalUtils
				.listMethodsNonRecursive(methodChildren);
		updateAssessmentForChildMethods(assessment, findingColorRegions,
				childMethods, element);

		int methodStartTokenIndex = method.getStartTokenIndex();
		int size = 0;
		ETrafficLightColor methodColor = ETrafficLightColor.GREEN;
		int firstChildTokenIndex = methodChildren.get(0).getStartTokenIndex();
		for (ShallowEntity childMethod : childMethods) {
			List<IToken> tokens = safeSubList(method.includedTokens(),
					firstChildTokenIndex - methodStartTokenIndex,
					childMethod.getStartTokenIndex() - methodStartTokenIndex,
					element);
			size += determineSize(tokens);
			methodColor = ETrafficLightColor.getDominantColor(methodColor,
					determineColor(tokens, findingColorRegions));

			firstChildTokenIndex = childMethod.getEndTokenIndex();
		}

		int lastChildTokenIndex = CollectionUtils.getLast(methodChildren)
				.getEndTokenIndex();
		List<IToken> tokens = safeSubList(method.includedTokens(),
				firstChildTokenIndex - methodStartTokenIndex,
				lastChildTokenIndex - methodStartTokenIndex, element);
		size += determineSize(tokens);
		methodColor = ETrafficLightColor.getDominantColor(methodColor,
				determineColor(tokens, findingColorRegions));

		assessment.add(methodColor, size);
	}

	/**
	 * Performs a sublist operation. If the given indexes are not valid, a
	 * smalled list is reduced. Invalid indexes can happen in case of parsing
	 * errors (non-compiling files) and a reduced list at least allows us to
	 * continue somehow. The problem is logged.
	 */
	private List<IToken> safeSubList(List<IToken> tokens, int fromIndex,
			int toIndex, ITokenElement element) {
		boolean hadInvalid = false;
		if (toIndex > tokens.size()) {
			toIndex = tokens.size();
			hadInvalid = true;
		}

		if (toIndex < fromIndex) {
			fromIndex = toIndex;
			hadInvalid = true;
		}

		if (hadInvalid && invalidFilesWarned.add(element.getUniformPath())) {
			getLogger().warn(
					"Encountered invalid token index in "
							+ element.getUniformPath()
							+ ". This is likely due to a non-compiling file.");
		}

		return tokens.subList(fromIndex, toIndex);
	}

	/** Runs the assessment on the given (sub) methods. */
	private void updateAssessmentForChildMethods(Assessment assessment,
			Map<ETrafficLightColor, RegionSet> findingColorRegions,
			List<ShallowEntity> childMethods, ITokenElement element) {
		for (ShallowEntity childMethod : childMethods) {
			updateAssessmentForMethod(assessment, childMethod,
					findingColorRegions, element);
		}
	}

	/**
	 * Returns the size of the region defined by the given tokens in LOC or
	 * SLOC.
	 */
	private int determineSize(List<IToken> tokens) {
		if (tokens.isEmpty()) {
			return 0;
		}

		if (slocBased) {
			String content = TokenElementUtils.getFilteredTokenContent(
					SLOCAnalyzer.NON_COMMENT_TOKEN_CLASSES, tokens);
			return StringUtils.countLines(content);
		}

		// LOC based size
		IToken lastToken = CollectionUtils.getLast(tokens);
		return lastToken.getLineNumber()
				+ StringUtils.countLines(lastToken.getText())
				- tokens.get(0).getLineNumber();
	}

	/**
	 * Returns the color to be used for the regions defined by the given tokens.
	 */
	private ETrafficLightColor determineColor(List<IToken> tokens,
			Map<ETrafficLightColor, RegionSet> findingColorRegions) {
		if (tokens.isEmpty()) {
			// will not be counted in any case, but we return GREEN
			return ETrafficLightColor.GREEN;
		}

		Region tokenRegion = new Region(tokens.get(0).getOffset(),
				CollectionUtils.getLast(tokens).getEndOffset());
		for (ETrafficLightColor color : ETrafficLightColor.values()) {
			RegionSet regions = findingColorRegions.get(color);
			if (regions != null && regions.containsAny(tokenRegion)) {
				// colors in ETrafficLightColor are ordered by severity, so
				// return first match
				return color;
			}
		}

		// no match -> GREEN
		return ETrafficLightColor.GREEN;
	}

}
