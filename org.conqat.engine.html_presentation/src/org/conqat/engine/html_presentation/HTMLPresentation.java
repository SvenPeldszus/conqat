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
package org.conqat.engine.html_presentation;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.commons.css.CSSMananger;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IShutdownHook;
import org.conqat.engine.html_presentation.base.ConfigGraphGenerator;
import org.conqat.engine.html_presentation.base.ConfigJSONWriter;
import org.conqat.engine.html_presentation.base.ConfigPageGenerator;
import org.conqat.engine.html_presentation.base.ExecutionTimeMapGenerator;
import org.conqat.engine.html_presentation.base.HeaderWriter;
import org.conqat.engine.html_presentation.base.IndexPageWriter;
import org.conqat.engine.html_presentation.base.LoggingPagesGenerator;
import org.conqat.engine.html_presentation.base.NavigationPageWriter;
import org.conqat.engine.html_presentation.base.OverviewPageWriter;
import org.conqat.engine.html_presentation.javascript.JavaScriptManager;
import org.conqat.engine.html_presentation.util.PageDescriptorList;
import org.conqat.engine.html_presentation.util.ResourcesManager;
import org.conqat.lib.commons.collections.ListMap;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49983 $
 * @ConQAT.Rating GREEN Hash: 9BD00DDA9F37BA6C0D2991682510E6F8
 */
@AConQATProcessor(description = "The processor is the core of the HTML presentation. "
		+ "It generates the navigation frames, the config graph and the logging pages. "
		+ "Actual results are not generated by this processor but by the various layouters, "
		+ "e.g. TableLayouter, GraphLayouter, TreeMapLayout in this bundle. Their results "
		+ "(IPageDescriptors) are forwarded to the presentation processor as parameters.")
public class HTMLPresentation extends ConQATProcessorBase {

	/** Name of the icon file for external pages */
	private static final String EXTERNAL_ICON = "external.gif";

	/** Group name of Log messages and config graph page. */
	public static final String INFO_GROUP_NAME = "Info";

	/**
	 * Special group name for pages that should not appear in the navigation
	 * frame.
	 */
	public static final String INVISIBLE_GROUP_NAME = "internal";

	/**
	 * Set of group names to exclude from overview page.
	 * {@link #INFO_GROUP_NAME} is added to this plus groups that were specified
	 * via {@link #addExcludedGroupName(String)}.
	 */
	private final Set<String> excludedGroupNames = new HashSet<String>();

	/**
	 * Results to display. This maps from the group name to a list of results
	 * associated with the group name. Linked map keeps order of addition.
	 */
	private final ListMap<String, IPageDescriptor> pageMap = new ListMap<String, IPageDescriptor>(
			new LinkedHashMap<String, List<IPageDescriptor>>());

	/** Project name */
	private String projectName = "ConQAT Dashboard";

	/** Number of columns on overview page. */
	private int overviewPageColumnCount = 2;

	/** Output directory. */
	private File outputDirectory;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "overview", attribute = "page", optional = true, description = "If provided, renders the given page below the auto-generated overview page.")
	public IPageDescriptor additionalOverviewPage = null;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "output", minOccurrences = 1, maxOccurrences = 1, description = "Output parameters")
	public void setOutputDirectory(
			@AConQATAttribute(name = "dir", description = "Name of the output directory.") String dir) {
		outputDirectory = new File(dir);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "exclude-from-overview", description = "Group to exclude from overview page")
	public void addExcludedGroupName(
			@AConQATAttribute(name = "groupID", description = "Name of the group to exclude") String groupName) {
		excludedGroupNames.add(groupName);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "result", description = "Add result to display")
	public void addPage(
			@AConQATAttribute(name = "ref", description = "Reference to the result descriptor") IPageDescriptor page) {

		if (page == null) {
			getLogger().warn("Ignoring null result received!");
		} else {
			pageMap.add(page.getGroupId(), page);
		}
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "result-list", description = "Add results to display")
	public void addPageList(
			@AConQATAttribute(name = "ref", description = "Reference to the result descriptor list") PageDescriptorList list) {
		for (IPageDescriptor page : list) {
			pageMap.add(page.getGroupId(), page);
		}
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "overview-page", minOccurrences = 0, maxOccurrences = 1, description = "Set overview page preferences")
	public void setColumnCount(
			@AConQATAttribute(name = "columns", description = "Number of columns [2]. Number of columns must be at least 1.") int columnCount)
			throws ConQATException {

		if (columnCount < 1) {
			throw new ConQATException("Number of columns must be at least 1.");
		}

		overviewPageColumnCount = columnCount;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "project", minOccurrences = 0, maxOccurrences = 1, description = "Project title")
	public void setProjectTitle(
			@AConQATAttribute(name = "title", description = "Project title.") String title) {
		projectName = title;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "external", description = "Adds a link to an external page", minOccurrences = 0, maxOccurrences = -1)
	public void addExternalPage(
			@AConQATAttribute(name = "url", description = "URL to external page.") String url,
			@AConQATAttribute(name = "name", description = "Name of the external page.") String name,
			@AConQATAttribute(name = "group", description = "Group of the external page.") String groupId) {
		IPageDescriptor page = new PageDescriptor("External page", name,
				groupId, EXTERNAL_ICON, url, true);
		pageMap.add(groupId, page);
	}

	/**
	 * Performs the actual processing.
	 * 
	 * @return nothing (i.e. a null pointer.)
	 */
	@Override
	public Object process() throws ConQATException {

		new ResourcesManager(outputDirectory).prepare();
		JavaScriptManager.getInstance()
				.copyScript(outputDirectory, getLogger());

		// exclude info group from overview page
		excludedGroupNames.add(INFO_GROUP_NAME);

		new OverviewPageWriter(outputDirectory, pageMap, excludedGroupNames,
				overviewPageColumnCount, additionalOverviewPage).write();
		new IndexPageWriter(outputDirectory, projectName).write();
		new HeaderWriter(outputDirectory, projectName).write();

		// register second stage as shutdown hook, to ensure all logs are
		// present
		getProcessorInfo().registerShutdownHook(new IShutdownHook() {
			@Override
			public void performShutdown() throws ConQATException {
				performSecondOutputStage();
			}
		}, true);

		// nothing to return. This is a sink!
		return null;
	}

	/** Performs the second stage of output writing. */
	private void performSecondOutputStage() throws ConQATException {
		createInfoPages();

		// generate navigation page
		new NavigationPageWriter(outputDirectory, pageMap).write();

		// generate all pages stored in pageMap. This includes log and
		// and config graph pages.
		generatePages();

		// write CSS
		CSSMananger.getInstance().write(outputDirectory);

		// dump abbreviations, which can be useful for debugging and will not
		// harm anyone
		dumpAbbreviations();

		getLogger().info(
				"Output written to: "
						+ new File(outputDirectory, "index.html")
								.getAbsolutePath());
	}

	/** Create pages that are displayed in the info section of the dashboard */
	private void createInfoPages() throws ConQATException {
		new ConfigJSONWriter(getProcessorInfo(), outputDirectory).writeAll();

		pageMap.add(INFO_GROUP_NAME, new LoggingPagesGenerator(
				getProcessorInfo().getLogManager()).createPage());

		pageMap.add(INFO_GROUP_NAME, new ConfigGraphGenerator(
				getProcessorInfo().getConfigurationInformation()).createPage());

		pageMap.add(INFO_GROUP_NAME, new ExecutionTimeMapGenerator(
				outputDirectory, getProcessorInfo()
						.getConfigurationInformation()).createPage());

		pageMap.add(INFO_GROUP_NAME,
				new ConfigPageGenerator(getProcessorInfo()).createPage());
	}

	/**
	 * Dumps the abbreviations as a CSV file in the output directory. This dump
	 * file can be used to lookup abbreviated names and may help in debugging.
	 */
	private void dumpAbbreviations() throws ConQATException {
		StringBuilder sb = new StringBuilder();
		for (Entry<String, String> e : BundleContext.getInstance()
				.getHtmlPresentationManager().getAbbreviations().entrySet()) {
			sb.append(e.getKey() + ";" + e.getValue() + StringUtils.CR);
		}
		try {
			FileSystemUtils.writeFile(new File(outputDirectory,
					"abbreviations.csv"), sb.toString());
		} catch (IOException e) {
			throw new ConQATException("Could not write abbreviations!", e);
		}
	}

	/** Iterate over all descriptors in {@link #pageMap} and generate pages. */
	private void generatePages() throws ConQATException {
		for (String groupName : pageMap.getKeys()) {
			List<IPageDescriptor> results = pageMap.getCollection(groupName);

			for (IPageDescriptor result : results) {
				if (!result.isExternal()) {
					new PageWriter(outputDirectory, result).write();
				}
			}
		}
	}

}