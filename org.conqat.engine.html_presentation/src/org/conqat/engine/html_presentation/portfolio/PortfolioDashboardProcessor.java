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
package org.conqat.engine.html_presentation.portfolio;

import java.awt.Color;
import java.io.IOException;
import java.text.DateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.conqat.engine.commons.ConQATProcessorBase;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.commons.node.StringSetNode;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.html_presentation.color.ColorizerBase;
import org.conqat.engine.html_presentation.javascript.config.StatisticsDataJson;
import org.conqat.engine.html_presentation.links.LinkProviderBase;
import org.conqat.engine.html_presentation.links.LinkTargetAssigner;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.color.ECCSMColor;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: streitel $
 * @version $Rev: 51294 $
 * @ConQAT.Rating YELLOW Hash: DFFF44BFB895A58C4FEC9932EDE90870
 */
@AConQATProcessor(description = "This processor retrieves dashboard statistics data via "
		+ "HTTP or the local filesystem and converts it to a IConQATNode structure that can be"
		+ "rendered with a TableLayouter.")
public class PortfolioDashboardProcessor extends ConQATProcessorBase {

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.Integer", description = "The number of errors in the dashboard.")
	public static final String KEY_ERRORS = "Errors";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "org.conqat.lib.commons.Assessment", description = "The assessment of the dashboard.")
	public static final String KEY_ASSESSMENT = "Assessment";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.Integer", description = "The number of warnings in the dashboard.")
	public static final String KEY_WARNINGS = "Warnings";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.String", description = "The number of milliseconds it took for the dashboard to execute.")
	public static final String KEY_EXECUTION_TIME = "Execution Time";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.String", description = "The last time the dashboard was executed.")
	public static final String KEY_LAST_EXECUTION = "Last Execution";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.util.Date", description = "The last time the dashboard was executed, as a Date object.")
	public static final String KEY_LAST_EXECUTION_DATE = "Last Execution Date";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.String", description = "The number of failed and executed processors of the dashboard.")
	public static final String KEY_NUMBER_OF_PROCESSORS = "Processors (failed/all)";

	/** {@ConQAT.Doc} */
	@AConQATKey(type = "java.lang.String", description = "The location of the dashboard.")
	public static final String KEY_LOCATION = "Location";

	/** The URLs of all dashboards that should be processed. */
	private final List<PortfolioDataFetcherBase> dashboards = new LinkedList<PortfolioDataFetcherBase>();

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "color", attribute = "key", description = ""
			+ "Defines the key from which the color can be looked up. Defaults to "
			+ ColorizerBase.COLOR_KEY_DEFAULT, optional = true)
	public String colorKey = ColorizerBase.COLOR_KEY_DEFAULT;

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "assessment", attribute = "key", description = ""
			+ "Defines the key from which the assessment can be looked up. Defaults to "
			+ KEY_ASSESSMENT, optional = true)
	public String assessmentKey = KEY_ASSESSMENT;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "http-dashboard", description = "Adds a dashboard that should be processed"
			+ " and can be accessed via HTTP.")
	public void addHttpDashboard(
			@AConQATAttribute(name = "url", description = "The location of the dashboard as an HTTP URL.") String locationUrl,
			@AConQATAttribute(name = "name", description = "The name of the dashboard.") String name) {
		dashboards.add(new PortfolioHttpDataFetcher(name, locationUrl));
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "http-auth-dashboard", description = "Adds a dashboard that should be processed"
			+ " and can be accessed via HTTP using basic authentication.")
	public void addHttpAuthenticatedDashboard(
			@AConQATAttribute(name = "url", description = "The location of the dashboard as an HTTP URL.") String locationUrl,
			@AConQATAttribute(name = "username", description = "The username used to access the URL via basic authentication.") String username,
			@AConQATAttribute(name = "password", description = "The password used to access the URL via basic authentication.") String password,
			@AConQATAttribute(name = "name", description = "The name of the dashboard.") String name) {
		dashboards.add(new PortfolioHttpAuthenticatedDataFetcher(name,
				locationUrl, username, password));
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "http-jenkins-dashboard", description = "Adds a dashboard that should be processed"
			+ " and can be accessed via HTTP behind a Jenkins server that requires a login.")
	public void addHttpJenkinsDashboard(
			@AConQATAttribute(name = "url", description = "The location of the dashboard as an HTTP URL.") String locationUrl,
			@AConQATAttribute(name = "jenkins-url", description = "The location of the Jenkins as an HTTP URL.") String jenkinsBaseUrl,
			@AConQATAttribute(name = "username", description = "The username used to access the Jenkins.") String username,
			@AConQATAttribute(name = "password", description = "The password used to access the Jenkins.") String password,
			@AConQATAttribute(name = "name", description = "The name of the dashboard.") String name) {
		dashboards.add(new PortfolioHttpJenkinsDataFetcher(name, locationUrl,
				jenkinsBaseUrl, username, password));
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "file-dashboard", description = "Adds a dashboard that should be processed"
			+ " and can be accessed via the local file system.")
	public void addFileDashboard(
			@AConQATAttribute(name = "dir", description = "The location of the dashboard on the local file system.") String path,
			@AConQATAttribute(name = "name", description = "The name of the dashboard.") String name) {
		dashboards.add(new PortfolioFileDataFetcher(name, path));
	}

	/** {@inheritDoc} */
	@Override
	public StringSetNode process() {
		StringSetNode root = new StringSetNode("Portfolio Dashboard");
		NodeUtils.setHideRoot(root, true);

		NodeUtils.addToDisplayList(root, KEY_ERRORS, KEY_WARNINGS,
				KEY_EXECUTION_TIME, KEY_LAST_EXECUTION,
				KEY_NUMBER_OF_PROCESSORS);

		for (PortfolioDataFetcherBase fetcher : dashboards) {
			StringSetNode node = new StringSetNode(fetcher.getDashboardName());
			try {
				StatisticsDataJson statistics = fetcher.retrieveData();
				setNodeValues(node, fetcher, statistics);
			} catch (IOException e) {
				handleError(fetcher, node, e);
			} catch (ConQATException e) {
				handleError(fetcher, node, e);
			}
			root.addChild(node);
		}
		return root;
	}

	/**
	 * Handles an exception by logging the error and adding an error node to the
	 * nodes.
	 */
	private void handleError(PortfolioDataFetcherBase fetcher,
			StringSetNode node, Exception e) {
		setErrorValues(node, fetcher);
		getLogger().error(
				"Unable to fetch data for dashboard "
						+ fetcher.getDashboardName() + " at "
						+ fetcher.getDashboardLocation() + ": "
						+ e.getMessage(), e);
	}

	/**
	 * Sets the given node's values in the case that an error occurred during
	 * fetching.
	 */
	private void setErrorValues(StringSetNode node,
			PortfolioDataFetcherBase fetcher) {
		NodeUtils.getOrCreateAssessment(node, assessmentKey).add(
				ETrafficLightColor.RED);
		node.setValue(colorKey, ECCSMColor.RED.getColor());
		setDashboardLocationKeys(node, fetcher);

		for (String key : Arrays.asList(KEY_ERRORS, KEY_WARNINGS,
				KEY_EXECUTION_TIME, KEY_LAST_EXECUTION,
				KEY_NUMBER_OF_PROCESSORS)) {
			node.setValue(key, "-unavailable-");
		}
	}

	/** Sets the values of the given node according to the given data. */
	private void setNodeValues(StringSetNode node,
			PortfolioDataFetcherBase fetcher, StatisticsDataJson statistics) {
		Color color;
		ETrafficLightColor trafficLightColor;
		if (statistics.getErrors() > 0
				|| statistics.getNumberOfFailedProcessors() > 0) {
			color = ECCSMColor.RED.getColor();
			trafficLightColor = ETrafficLightColor.RED;
		} else if (statistics.getWarnings() > 0) {
			color = ECCSMColor.YELLOW.getColor();
			trafficLightColor = ETrafficLightColor.YELLOW;
		} else {
			color = ECCSMColor.GREEN.getColor();
			trafficLightColor = ETrafficLightColor.GREEN;
		}
		NodeUtils.getOrCreateAssessment(node, assessmentKey).add(
				trafficLightColor);
		node.setValue(colorKey, color);

		setDashboardLocationKeys(node, fetcher);

		node.setValue(KEY_ERRORS, statistics.getErrors());
		node.setValue(KEY_WARNINGS, statistics.getWarnings());
		node.setValue(KEY_EXECUTION_TIME, statistics.getExecutionTime());
		Date lastExecutionDate = new Date(statistics.getLastExecution());
		node.setValue(KEY_LAST_EXECUTION_DATE, lastExecutionDate);
		String lastExecutionDateString = DateFormat.getDateTimeInstance()
				.format(lastExecutionDate);
		node.setValue(KEY_LAST_EXECUTION, lastExecutionDateString);
		node.setValue(
				KEY_NUMBER_OF_PROCESSORS,
				statistics.getNumberOfFailedProcessors() + "/"
						+ statistics.getNumberOfProcessors());
	}

	/**
	 * Sets two keys: one that contains a link to the dashboard (used by the
	 * {@link LinkTargetAssigner}) and one that contains the location of the
	 * dashboard, which can be used by subsequent processors to fetch further
	 * information from the dashboard to modify the node's assessment.
	 */
	private void setDashboardLocationKeys(StringSetNode node,
			PortfolioDataFetcherBase fetcher) {
		node.setValue(LinkProviderBase.LINK_KEY, fetcher.getDashboardLocation()
				+ "/index.html");
		node.setValue(KEY_LOCATION, fetcher.getDashboardLocation());
	}
}
