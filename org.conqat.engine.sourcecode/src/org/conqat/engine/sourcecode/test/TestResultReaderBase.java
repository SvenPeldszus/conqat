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
package org.conqat.engine.sourcecode.test;

import java.util.List;

import org.conqat.engine.commons.format.EValueFormatter;
import org.conqat.engine.commons.node.DisplayList;
import org.conqat.engine.commons.node.IConQATNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.util.ConQATInputProcessorBase;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;

/**
 * Base class for reading unit test result files.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50687 $
 * @ConQAT.Rating RED Hash: 060A0D9D6223CAD77F31D231B133AA73
 */
public abstract class TestResultReaderBase<ROOT extends IConQATNode> extends
		ConQATInputProcessorBase<ITextResource> {

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The assessment for the test case.", type = "org.conqat.lib.commons.assessment.Assessment")
	public static final String ASSESSMENT_KEY = "assessment";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "The execution time of the test case in seconds.", type = "java.lang.Double")
	public static final String EXECUTION_TIME_KEY = "execution time";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "A description of the test result.", type = "java.lang.String")
	public static final String RESULT_KEY = "test result";

	/** The test root which test results get attached to. */
	protected final ROOT root;

	/** Constructor. */
	public TestResultReaderBase(ROOT root) {
		this.root = root;
	}

	/** {@inheritDoc} */
	@Override
	public ROOT process() throws ConQATException {
		setUp();

		List<ITextElement> textElements = ResourceTraversalUtils
				.listTextElements(input);

		if (textElements.size() == 0) {
			getLogger().warn("No test result files found.");
		}

		for (ITextElement textElement : textElements) {
			parseTextElement(textElement);
		}

		return root;
	}

	/** Sets up the root node by configuring its display list. */
	protected void setUp() {
		DisplayList displayList = NodeUtils.getDisplayList(root);

		displayList.addKey(ASSESSMENT_KEY, null);
		displayList.addKey(EXECUTION_TIME_KEY,
				EValueFormatter.FIXED_1.getFormatter());
		displayList.addKey(RESULT_KEY, null);

		NodeUtils.setHideRoot(root, true);
	}

	/**
	 * Parses the test results of the text element and attaches them to the test
	 * {@link #root}.
	 */
	protected abstract void parseTextElement(ITextElement textElement)
			throws ConQATException;

	/**
	 * Parameter object that defines the strategy for namespace separators in
	 * test result IDs.
	 * <p>
	 * In most C-like languages namespaces are separated by a dot, e.g.
	 * <code>org.conqat.engine</code>. The same applies for unit test methods.
	 * They are usually identified by
	 * <code>na.me.spa.ce.ClassName.MethodName</code>. This ID strategy allows
	 * to optionally change the dot to a slash, e.g.
	 * <code>na/me/spa/cd/ClassName/MethodName</code>, so the node ID looks like
	 * a uniform path with folders. This is needed as some tools (e.g.
	 * Teamscale) visualize nodes as folders.
	 */
	public static class ResultIdStrategy implements IConQATParameterObject {

		/** {@ConQAT.Doc} */
		// TODO (FS) the parameter name is confusing. when turned on, the
		// separator is actually NOT a slash but a dot, which is replaced by a
		// slash. please rename
		// TODO (MP) No, when true the separator is a slash.
		// TODO (FS) I would still prefer this be called something like
		// 'make-separator-slash' or 'replace-dot-with-slash' etc. 'is slash'
		// sounds like you are describing the
		// input, not the desired output
		// Other option: make this parameter take a string: the separator. the
		// default value would be '.' and if the user desires, they can replace
		// it with a '/'. This would be more intuitive when using the processor
		// in ConQAT IMO
		@AConQATFieldParameter(parameter = "separator-is-slash", attribute = "value", optional = true, description = ""
				+ "If this is true, dots in the full qualified  method name are replaced by a slash for a more file system like presentation. "
				+ "E.g. na.me.spa.ce.ClassName.MethodName becomes na/me/spa/ce/ClassName/MethodName. Default is false.")
		public boolean isSlash = false;

		/**
		 * Returns the node ID to use for the given namespace and name by
		 * concatenating the two parts with the configured separator. If
		 * {@link #isSlash} is true all dots in the resulting node id will be
		 * replaced by slashes. Otherwise dots are left as are.
		 */
		public String determineId(String namespace, String name) {
			String id = namespace + "." + name;
			if (isSlash) {
				id = id.replace('.', '/');
			}
			return id;
		}

		/**
		 * Returns the separator configured with the current id strategy
		 * according to {@link #isSlash}.
		 */
		public String getIdSeparator() {
			if (isSlash) {
				return "/";
			}
			return ".";
		}
	}
}
