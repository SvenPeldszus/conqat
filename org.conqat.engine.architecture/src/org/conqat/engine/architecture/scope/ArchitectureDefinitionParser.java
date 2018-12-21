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
package org.conqat.engine.architecture.scope;

import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.DIM;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.NAME;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.POS;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.REGEX;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.SOURCE;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.STEREOTYPE;
import static org.conqat.engine.architecture.format.EArchitectureIOAttribute.TARGET;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.ALLOW;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.CODE_MAPPING;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.COMMENT;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.COMPONENT;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.DENY;
import static org.conqat.engine.architecture.format.EArchitectureIOElement.TOLERATE;
import static org.conqat.engine.architecture.format.ECodeMappingType.EXCLUDE;
import static org.conqat.engine.architecture.format.ECodeMappingType.INCLUDE;

import java.awt.Dimension;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.architecture.assessment.shared.TypeDependency;
import org.conqat.engine.architecture.format.ArchitectureFormats;
import org.conqat.engine.architecture.format.EArchitectureIOAttribute;
import org.conqat.engine.architecture.format.EArchitectureIOElement;
import org.conqat.engine.architecture.format.EStereotype;
import org.conqat.engine.commons.architecture.EPolicyType;
import org.conqat.engine.commons.util.ConQATXMLReader;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.enums.EnumUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.xml.IXMLElementProcessor;
import org.conqat.lib.commons.xml.LowercaseResolver;

/**
 * Parses XML architecture definition files and resources into
 * {@link ArchitectureDefinition} objects.
 * 
 * @author $Author: pawelka $
 * @version $Rev: 51693 $
 * @ConQAT.Rating YELLOW Hash: B3458B6D47505A5B1279601C49B865F4
 */
public class ArchitectureDefinitionParser
		extends
			ConQATXMLReader<EArchitectureIOElement, EArchitectureIOAttribute, ConQATException> {

	/** Regular expression that matches digits potentially prefixed with "-" */
	private static final String DIGITS = "(-?\\d+)";

	/** Pattern used to extract positions and dimensions. */
	private static final Pattern NUMBER_PAIR_PATTERN = Pattern.compile(DIGITS
			+ ", ?" + DIGITS);

	/**
	 * Key for the system property that allows to control how architectures are
	 * handled that do not have the flag
	 * {@link EArchitectureIOAttribute#FILE_BASED} set.
	 */
	private static final String ARCHITECTURE_ASSESSMENT_DEFAULT_IS_FILE_BASED = "architecture.default.file-based";

	/** The location of the parsed XML document. */
	private final String location;

	/** The name of the architecture definition */
	private final String architectureName;

	/** Mapping from component name to component. */
	private final Map<String, ComponentNode> componentByName = new HashMap<String, ComponentNode>();

	/** Constructor. */
	public ArchitectureDefinitionParser(File file) throws IOException {
		super(
				file,
				new LowercaseResolver<EArchitectureIOElement, EArchitectureIOAttribute>(
						EArchitectureIOAttribute.class));
		setSchema(ArchitectureFormats.getArchitectureDefinitionSchema());
		location = file.getAbsolutePath();
		architectureName = file.getName();
	}

	/** Constructor. */
	public ArchitectureDefinitionParser(ITextElement textElement)
			throws ConQATException {
		super(
				textElement.getUnfilteredTextContent(),
				new LowercaseResolver<EArchitectureIOElement, EArchitectureIOAttribute>(
						EArchitectureIOAttribute.class));
		setSchema(ArchitectureFormats.getArchitectureDefinitionSchema());
		location = textElement.getLocation();
		architectureName = textElement.getUniformPath();
	}

	/**
	 * Parses the architecture definition XML and returns an
	 * {@link ArchitectureDefinition}.
	 */
	public ArchitectureDefinition parse() throws ConQATException {
		parseAndWrapExceptions();

		ArchitectureDefinition result = new ArchitectureDefinition(
				architectureName, determineFileBasedFlag(),
				getStringAttribute(EArchitectureIOAttribute.SCOPE_INCLUDE),
				getStringAttribute(EArchitectureIOAttribute.SCOPE_EXCLUDE));

		processChildElements(new ComponentProcessor(result));
		processChildElements(new PolicyProcessor(ALLOW,
				EPolicyType.ALLOW_EXPLICIT));
		processChildElements(new PolicyProcessor(DENY,
				EPolicyType.DENY_EXPLICIT));
		processChildElements(new PolicyProcessor(TOLERATE,
				EPolicyType.TOLERATE_EXPLICIT));

		return result;
	}

	/**
	 * Determines the value of the {@link EArchitectureIOAttribute#FILE_BASED}
	 * flag. This allows additionally checks a system property that allows to
	 * override the default. This allows to control how old architectures are
	 * handled that do not have the flag set.
	 */
	private boolean determineFileBasedFlag() {
		if (!hasAttribute(EArchitectureIOAttribute.FILE_BASED)) {
			// check system property for architectures without flag
			return Boolean
					.getBoolean(ARCHITECTURE_ASSESSMENT_DEFAULT_IS_FILE_BASED);
		}
		return getBooleanAttribute(EArchitectureIOAttribute.FILE_BASED);
	}

	/** {@inheritDoc} */
	@Override
	protected String getLocation() {
		return location;
	}

	/** Insert single dependency rule into the component tree. */
	private DependencyPolicy insertPolicy(String sourceName, String targetName,
			EPolicyType policyType, List<Point> points) throws ConQATException {
		return createPolicy(getComponent(sourceName), getComponent(targetName),
				policyType, points);
	}

	/**
	 * Returns the component of given name. Throws an exception if the component
	 * was not found.
	 */
	private ComponentNode getComponent(String name) throws ConQATException {
		ComponentNode component = componentByName.get(name);
		if (component == null) {
			throw new ConQATException("Component " + name + " not found!");
		}
		return component;
	}

	/** Creates a policy. */
	private DependencyPolicy createPolicy(ComponentNode source,
			ComponentNode target, EPolicyType policyType, List<Point> points)
			throws ConQATException {
		DependencyPolicy policy = new DependencyPolicy(source, target,
				policyType, points);
		policy.registerWithComponents();
		return policy;
	}

	/**
	 * Parses a pair of integer numbers from a string and throws an exception if
	 * it does not match the parsing pattern.
	 */
	private static Pair<Integer, Integer> parseNumberPair(String pairString)
			throws ConQATException {
		Matcher matcher = NUMBER_PAIR_PATTERN.matcher(pairString);
		if (!matcher.matches()) {
			throw new ConQATException("Invalid position/dimension string: "
					+ pairString);
		}
		return new Pair<>(Integer.parseInt(matcher.group(1)),
				Integer.parseInt(matcher.group(2)));
	}

	/** Processor for components. */
	private class ComponentProcessor
			implements
				IXMLElementProcessor<EArchitectureIOElement, ConQATException> {

		/** The parent node. */
		private final ComponentNode parent;

		/** Constructor. */
		public ComponentProcessor(ComponentNode parent) {
			this.parent = parent;
		}

		/** {@inheritDoc} */
		@Override
		public EArchitectureIOElement getTargetElement() {
			return COMPONENT;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			String name = getStringAttribute(NAME);
			if (componentByName.containsKey(name)) {
				throw new ConQATException("Duplicate unique name: " + name);
			}

			String stereotypeString = getStringAttribute(STEREOTYPE);
			EStereotype stereoType = EStereotype.NONE;
			if (!StringUtils.isEmpty(stereotypeString)) {
				stereoType = EnumUtils.valueOfIgnoreCase(EStereotype.class,
						stereotypeString);
			}

			Pair<Integer, Integer> positionPair = parseNumberPair(getStringAttribute(POS));
			Pair<Integer, Integer> dimensionPair = parseNumberPair(getStringAttribute(DIM));
			Point position = new Point(positionPair.getFirst(),
					positionPair.getSecond());
			Dimension dimension = new Dimension(dimensionPair.getFirst(),
					dimensionPair.getSecond());

			ComponentNode node = new ComponentNode(name, position, dimension,
					stereoType);

			componentByName.put(name, node);

			processChildElements(new ComponentProcessor(node));
			processChildElements(new CodeMappingProcessor(node));
			processChildElements(new CommentProcessor(node));

			parent.addChild(node);
		}

	}

	/** Processor for code mappings. */
	private final class CodeMappingProcessor
			implements
				IXMLElementProcessor<EArchitectureIOElement, ConQATException> {

		/** The parent node. */
		private final ComponentNode node;

		/** Constructor. */
		public CodeMappingProcessor(ComponentNode node) {
			this.node = node;
		}

		/** {@inheritDoc} */
		@Override
		public EArchitectureIOElement getTargetElement() {
			return CODE_MAPPING;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			String type = getStringAttribute(EArchitectureIOAttribute.TYPE);
			if (type.equalsIgnoreCase(INCLUDE.toString())) {
				node.addIncludeRegex(getStringAttribute(REGEX));
			} else if (type.equalsIgnoreCase(EXCLUDE.toString())) {
				node.addExcludeRegex(getStringAttribute(REGEX));
			} else {
				CCSMAssert
						.fail("The schema should check whether the node type is either an include or exclude.");
			}
		}
	}

	/** Processor for comments. */
	private final class CommentProcessor
			implements
				IXMLElementProcessor<EArchitectureIOElement, ConQATException> {

		/** The parent node. */
		private final ComponentNode node;

		/** Constructor. */
		public CommentProcessor(ComponentNode node) {
			this.node = node;
		}

		/** {@inheritDoc} */
		@Override
		public EArchitectureIOElement getTargetElement() {
			return COMMENT;
		}

		/** {@inheritDoc} */
		@Override
		public void process() {
			String description = getText();
			CCSMAssert.isNotNull(description);
			node.setDescription(description);
		}
	}

	/** Processor for policies. */
	private final class PolicyProcessor
			implements
				IXMLElementProcessor<EArchitectureIOElement, ConQATException> {

		/** Target element */
		private final EArchitectureIOElement targetElement;

		/** Type of policy being read */
		private final EPolicyType policyType;

		/** Creates PolicyProcessor for a policy type */
		public PolicyProcessor(final EArchitectureIOElement targetElement,
				EPolicyType policyType) {
			this.targetElement = targetElement;
			this.policyType = policyType;
		}

		/** {@inheritDoc} */
		@Override
		public EArchitectureIOElement getTargetElement() {
			return targetElement;
		}

		/** {@inheritDoc} */
		@Override
		public void process() throws ConQATException {
			DependencyPolicy policy = insertPolicy(getStringAttribute(SOURCE),
					getStringAttribute(TARGET), policyType, parsePoints());
			if (policyType == EPolicyType.TOLERATE_EXPLICIT) {
				processChildElements(new ToleratePolicyProcessor(policy));
			}
		}

		/** Parses the points attribute of a policy. */
		private List<Point> parsePoints() throws ConQATException {
			List<Point> points = new ArrayList<>();
			String pointsString = getStringAttribute(EArchitectureIOAttribute.POINTS);
			if (StringUtils.isEmpty(pointsString)) {
				return points;
			}

			String[] parts = pointsString.split(",");
			for (int i = 0; i + 1 < parts.length; i += 2) {
				try {
					points.add(new Point(Integer.parseInt(parts[i]), Integer
							.parseInt(parts[i + 1])));
				} catch (NumberFormatException e) {
					throw new ConQATException("Had invalid point description: "
							+ pointsString, e);
				}
			}
			return points;
		}
	}

	/** Processor for policies. */
	private final class ToleratePolicyProcessor
			implements
				IXMLElementProcessor<EArchitectureIOElement, ConQATException> {

		/** The parent policy we work on */
		private final DependencyPolicy policy;

		/** Constructor. */
		public ToleratePolicyProcessor(DependencyPolicy policy) {
			this.policy = policy;
		}

		/** {@inheritDoc} */
		@Override
		public EArchitectureIOElement getTargetElement() {
			return EArchitectureIOElement.DEPENDENCY;
		}

		/** {@inheritDoc} */
		@Override
		public void process() {
			policy.addToleratedTypeDependency(new TypeDependency(
					getStringAttribute(EArchitectureIOAttribute.SOURCE),
					getStringAttribute(EArchitectureIOAttribute.TARGET)));
		}
	}

}
