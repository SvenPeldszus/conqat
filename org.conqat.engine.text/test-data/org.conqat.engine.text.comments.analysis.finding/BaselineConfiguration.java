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
package com.teamscale.ide.prefs;

import java.io.IOException;
import java.util.Map;

import org.conqat.engine.service.shared.XmlSerializationUtils;
import org.conqat.ide.commons.ui.logging.LoggingUtils;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.eclipse.jface.preference.IPreferenceStore;

import com.teamscale.ide.TeamscaleIDEActivator;

/**
 * Class for storing the baseline configuration options, i. e. baseline and
 * include-changed-code-findings. Provides static methods for storing to and
 * loading from Eclipse preference store.
 * 
 * @author $Author: steidl $
 * @version $Rev: 48083 $
 * @ConQAT.Rating YELLOW Hash: B55371EEE2D387EE180DD735236602D5
 */
public class BaselineConfiguration {

	/** The preferences key used to store the configurations. */
	private static final String BASELINE_CONFIGURATIONS_KEY = "baselineConfigurations";

	/** The symbolic name of the baseline. */
	private final String baselineName;

	/** If existing findings in baseline should be shown. */
	private final boolean includeChangedCodeFindings;

	/** Constructor. */
	public BaselineConfiguration(String baselineName,
			boolean includeChangedCodeFindings) {
		this.baselineName = baselineName;
		this.includeChangedCodeFindings = includeChangedCodeFindings;
	}

	/** Returns the baseline name. */
	public String getBaselineName() {
		return baselineName;
	}

	/** Returns includeChangedCodeFindings */
	public boolean isIncludeChangedCodeFindings() {
		return includeChangedCodeFindings;
	}

	/**
	 * Store the configurations in the given preference store.
	 * 
	 * @param baselineConfigurationsMap
	 *            A map of project names to baseline configurations.
	 */
	public static void store(
			Map<String, BaselineConfiguration> baselineConfigurationsMap,
			IPreferenceStore store) {
		store.setValue(BASELINE_CONFIGURATIONS_KEY,
				XmlSerializationUtils.serializeToXML(baselineConfigurationsMap));
	}

	/**
	 * @param projectName
	 *            Name of the Teamscale project.
	 * @return The configured {@link BaselineConfiguration} or, if there is
	 *         none, an "empty" BaselineConfiguration with the baseline name set
	 *         to <code>null</code>.
	 */
	public static BaselineConfiguration getForProject(String projectName) {
		BaselineConfiguration baselineConfiguration = load().get(projectName);
		if (baselineConfiguration != null) {
			return baselineConfiguration;
		}
		return new BaselineConfiguration(null, false);
	}

	/**
	 * Load configurations from plugin preferences store.
	 * 
	 * @see #load(IPreferenceStore)
	 */
	public static Map<String, BaselineConfiguration> load() {
		return load(TeamscaleIDEActivator.getDefault().getPreferenceStore());
	}

	/**
	 * Load configurations from given preference store. Will return an empty map
	 * if no configuration could be read.
	 * 
	 * @return A map of project names with their corresponding baseline
	 *         configurations.
	 */
	@SuppressWarnings("unchecked")
	/* package */ public static Map<String, BaselineConfiguration> load(
			IPreferenceStore store) {
		String baselineConfigurationsXml = store
				.getString(BASELINE_CONFIGURATIONS_KEY);
		if (!StringUtils.isEmpty(baselineConfigurationsXml)) {
			try {
				return XmlSerializationUtils.deserializeFromXML(
						baselineConfigurationsXml, Map.class);
			} catch (IOException e) {
				LoggingUtils.error(TeamscaleIDEActivator.getDefault(), e);
			}
		}

		return CollectionUtils.emptyMap();
	}
	
	/** unrelated */
	public void related(){
		
	}
}