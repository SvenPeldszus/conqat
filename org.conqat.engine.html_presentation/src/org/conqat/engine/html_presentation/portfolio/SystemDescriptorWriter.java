/*-----------------------------------------------------------------------+
 | com.munichre.conqat.engine
 |                                                                       |
   $Id: SystemDescriptorWriter.java 48795 2014-03-27 13:09:46Z poehlmann $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package org.conqat.engine.html_presentation.portfolio;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.conqat.engine.commons.CommonUtils;
import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.util.ConQATInputProcessorBase;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.date.DateUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 48795 $
 * @ConQAT.Rating YELLOW Hash: 280A62253AEC82CB7491F6C1B1D61C83
 */
@AConQATProcessor(description = "Writes a system descriptor for a source scope that can be used in the portfolio dashboard or benchmark. This processor is a sink and has a null output.")
public class SystemDescriptorWriter extends
		ConQATInputProcessorBase<ITextResource> {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "output", attribute = "dir", description = "The directory to write the output into.")
	public String outputDir;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "date", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "The date of the source code. If omitted, the current date will be used.", optional = true)
	public Date date = DateUtils.getNow();

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "tag-open-source", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "Flag that determines whether the system is open source. Default: false", optional = true)
	public boolean tagOpenSource = false;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "tag-quality-controlled", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "Flag that determines whether the system is quality controlled. Default: false", optional = true)
	public boolean tagQualityControlled = false;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "write-scope-pattern", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "Flag that determines whether cqr patterns for file inclusion and generated code should be written. Default: false", optional = true)
	public boolean writeScopePattern = false;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "company", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "The name of the company that owns this system.")
	public String company;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "domain", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "The domain of the system, e.g. Business.")
	public String domain;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "language", attribute = ConQATParamDoc.VALUE_KEY_NAME, description = "The language of the system")
	public ELanguage language;

	/** The list of cqr parameters. */
	private PairList<String, String> cqrParams = new PairList<>();

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "cqr-param", description = "ConQAT CQR parameter/value pair that will be used for running the benchmark.")
	public void addCqrParameter(
			@AConQATAttribute(name = "param", description = "The CQR parameter name.") String param,
			@AConQATAttribute(name = "value", description = "The CQR parameter value.") String value) {
		cqrParams.add(param, value);
	}

	/** {@inheritDoc} */
	@Override
	public Object process() throws ConQATException {
		File descriptor = new File(outputDir, "system.xml");

		List<String> lines = new ArrayList<String>();
		lines.add("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");
		writeSystemInformation(lines);
		writeBooleanTag(lines, "open-source", tagOpenSource);
		writeBooleanTag(lines, "quality-controlled", tagQualityControlled);
		writeCqrParams(lines);
		lines.add("</system>");

		try {

			FileSystemUtils.writeFile(descriptor,
					StringUtils.concat(lines, "\n"));
		} catch (IOException e) {
			throw new ConQATException(e);
		}

		return null; // this is a sink.
	}

	/** Writes information regarding the system. */
	private void writeSystemInformation(List<String> lines)
			throws ConQATException {
		SimpleDateFormat dateFormat = CommonUtils
				.createDateFormat(CommonUtils.DEFAULT_DATE_FORMAT_PATTERN);

		ITextResource[] children = input.getChildren();
		CCSMAssert.isTrue(children.length == 1,
				"The resource must have exactly one root.");
		ITextResource root = children[0];
		lines.add("<system xmlns=\"http://www.cqse.eu/ns/benchmark\"");
		lines.add(toAttributeString("name", root.getName()));
		lines.add(toAttributeString("language", language.name()));
		lines.add(toAttributeString("company", company));
		lines.add(toAttributeString("domain", domain));
		lines.add(toAttributeString("version", dateFormat.format(date)));
		lines.add(toAttributeString("date", dateFormat.format(date)));
		lines.add(">");
		lines.add("<information></information>");
	}

	/** Writes the CQR Parameter section. */
	private void writeCqrParams(List<String> lines) {
		if (writeScopePattern) {
			// Add file includes to CQR params
			for (IElement element : ResourceTraversalUtils.listElements(input)) {
				String path = UniformPathUtils.stripProject(element
						.getUniformPath());
				addCqrParameter("file-include.pattern", path);

				if (NodeUtils.isIgnored(element, "ignore")) {
					addCqrParameter("generated-code-path.regex", path);
				}
			}
		}

		lines.add("<cqr-params>");

		for (int i = 0; i < cqrParams.size(); i++) {
			lines.add(cqrParams.getFirst(i) + "=" + cqrParams.getSecond(i));
		}

		lines.add("</cqr-params>");
	}

	/** Adds a boolean tag. */
	private static void writeBooleanTag(List<String> lines, String name,
			boolean bool) {
		String value = "no";
		if (bool) {
			value = "yes";
		}
		lines.add("<tag name=\"" + name + "\" value=\"" + value + "\" />");
	}

	/** Creates an attribute value pair out of two strings. */
	private static String toAttributeString(String name, String value) {
		return name + "=\"" + value + "\"";
	}
}
