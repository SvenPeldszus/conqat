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
package org.conqat.engine.architecture.scope;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.architecture.assessment.shared.IComponent;
import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;

/**
 * This processor reads an architecture definition from an XML file and creates
 * a {@link ComponentNode} hierarchy that represents the architecture.
 * 
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 49967 $
 * @ConQAT.Rating YELLOW Hash: 7213EEEB9EF184F0B3EFA7D2A657782D
 */
@AConQATProcessor(description = "This processor reads an architecture "
		+ "definition from a given input file (using our proprietary XML format) and creates "
		+ "a ComponentNode hierarchy that represents the architecture.")
public class ArchitectureDefinitionReader extends ConQATProcessorBase {

	/** Input file containing the xml architecture description */
	private File architectureFile;

	/** Input scope containing one xml file with the architecture description. */
	private ITextElement architectureTextElement;

	/** Root node of resulting architecture hierarchy */
	private ArchitectureDefinition result;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "input", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "Name of the input XML file describing the architecture.")
	public void setInputFile(
			@AConQATAttribute(name = "file", description = "The name of the XML file.") File architectureFile)
			throws ConQATException {
		if (!architectureFile.canRead() || !architectureFile.isFile()) {
			throw new ConQATException("Cannot read architecture file '"
					+ architectureFile + "'");
		}
		this.architectureFile = architectureFile;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "input-scope", minOccurrences = 0, maxOccurrences = 1, description = ""
			+ "TextResource Scope containing exactly one architecture definition file.")
	public void setInputScope(
			@AConQATAttribute(name = ConQATParamDoc.INPUT_REF_NAME, description = ConQATParamDoc.INPUT_REF_DESC) ITextResource textResource)
			throws ConQATException {
		List<ITextElement> textElements = ResourceTraversalUtils
				.listTextElements(textResource);
		if (textElements.size() != 1) {
			throw new ConQATException(
					"The architecture scope must include exactly one file.");
		}
		architectureTextElement = textElements.get(0);
	}

	/** Read the architecture definition from the given file. */
	@Override
	public ArchitectureDefinition process() throws ConQATException {

		if (architectureFile == null && architectureTextElement == null) {
			throw new ConQATException(
					"Neither an architecture file nor an architecture text scope is specified.");
		}

		try {
			ArchitectureDefinitionParser parser;
			if (architectureFile != null) {
				getLogger().debug(
						"Reading architecture definition file: "
								+ architectureFile.getAbsolutePath());
				parser = new ArchitectureDefinitionParser(architectureFile);
			} else {
				getLogger().debug(
						"Reading architecture definition file: "
								+ architectureTextElement.getLocation());
				parser = new ArchitectureDefinitionParser(
						architectureTextElement);
			}
			result = parser.parse();
		} catch (MalformedURLException e) {
			throw new IllegalStateException(
					"This can only happen due to missing resources.", e);
		} catch (IOException e) {
			throw new ConQATException("An i/o error occured!", e);
		}

		checkDependencies();

		return result;
	}

	/** Perform some sanity checks on dependencies. */
	private void checkDependencies() throws ConQATException {
		List<DependencyPolicy> policies = new ArrayList<DependencyPolicy>();
		result.collectPolicies(policies);

		checkTreeFollowing(policies);
		checkDuplicateAndCrossingEdges(policies);
	}

	/**
	 * Checks for policies that follow the hierarchy tree. These policies are
	 * unnecessary, since dependencies between parents and successors, or vice
	 * versa, are always allowed.
	 */
	private void checkTreeFollowing(List<DependencyPolicy> policies)
			throws ConQATException {
		for (DependencyPolicy policy : policies) {
			if (isTreeFollowing(policy)) {
				throw new ConQATException("Tree-following policy: " + policy);
			}
		}
	}

	/**
	 * Checks if the given policy is "tree-following", i.e. source or target is
	 * a parent of the other.
	 */
	private boolean isTreeFollowing(DependencyPolicy policy) {
		IComponent source = policy.getSource();
		IComponent target = policy.getTarget();
		return source.getAncestors().contains(target)
				|| target.getAncestors().contains(source);
	}

	/**
	 * Check for duplicate and crossing edges. The latter edges have no clear
	 * semantics and are thus illegal.
	 */
	// TODO (FS) please describe what "crossing" means
	// TODO (MP) Tbh, I do not know. Also see comment below, I'm not sure if our
	// code is working / properly documented. The best would be to peer-review
	// the part below.
	private void checkDuplicateAndCrossingEdges(List<DependencyPolicy> policies)
			throws ConQATException {
		for (DependencyPolicy policy : policies) {
			for (DependencyPolicy policy2 : policies) {
				if (policy != policy2) {
					checkNotSame(policy, policy2);
					checkNotCrossing(policy, policy2);
					checkNotCrossing(policy2, policy);
				}
			}
		}
	}

	/** Make sure the policies differ in at least source or target. */
	private void checkNotSame(DependencyPolicy policy, DependencyPolicy policy2)
			throws ConQATException {
		if (policy.getSource() == policy2.getSource()
				&& policy.getTarget() == policy2.getTarget()) {
			throw new ConQATException("Duplicate policy for " + policy);
		}
	}

	/** Make sure the two policies are not crossing. */
	private void checkNotCrossing(DependencyPolicy policy,
			DependencyPolicy policy2) throws ConQATException {
		boolean source2IsLower = policy.getSource().getAncestors()
				.contains(policy2.getSource());

		// TODO (MP) we are not referring to policy2.getTarget, so I am not sure
		// why the variable is named target2IsHigher?!
		boolean target2IsHigher = policy2.getSource().getAncestors()
				.contains(policy.getTarget());

		if (source2IsLower && target2IsHigher) {
			String message = "The policies " + policy + " and " + policy2
					+ " are crossing each other!";
			throw new ConQATException(message);
		}
	}
}