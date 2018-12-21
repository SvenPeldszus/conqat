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
| distributed under the License is distributed on an "AS IS" BASIS,        |  §copyright§
| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. |
| See the License for the specific language governing permissions and      |
| limitations under the License.                                           |
+-------------------------------------------------------------------------*/
package org.conqat.engine.index.repository.svn;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.index.configuration.ProjectConfigurationException;
import org.conqat.engine.index.configuration.model.AConfigExposed;
import org.conqat.engine.index.configuration.model.AccountCredentials;
import org.conqat.engine.index.configuration.model.ConnectorValidationException;
import org.conqat.engine.index.configuration.model.EConfigVisibility;
import org.conqat.engine.index.configuration.project.TriggerBuilder;
import org.conqat.engine.index.repository.base.NumericRevisionRepositoryConnectorDescriptorBase;
import org.tmatesoft.svn.core.SVNErrorCode;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.io.SVNRepository;

/**
 * Configuration descriptor for SVN.   §header§
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49617 $
 * @ConQAT.Rating GREEN Hash: BA703EA43305BB5F4B275AC6F5D2832F
 */
public class SVNRepositoryConnectorDescriptor extends
		NumericRevisionRepositoryConnectorDescriptorBase {

	/**
	 * The sub paths in the repository to include in the analysis. If left  §interface§
	 * empty, the whole repository is analyzed.
	 */
	@AConfigExposed(name = "Explicit sub-paths", visibility = EConfigVisibility.EXPERT, description = "The sub paths in the repository to include in the analysis. If left empty, the whole repository is analyzed.")
	private List<String> explicitPaths = new ArrayList<String>();

	/**
	 * The names of directories to exclude from the analysis. If left empty,  §interface§
	 * 'tags' and 'branches' will be excluded.
	 */
	@AConfigExposed(name = "Excluded directories", visibility = EConfigVisibility.EXPERT, description = "The names of directories to exclude from the analysis. If left empty, 'tags' and 'branches' will be excluded.")
	private List<String> excludedDirectories = new ArrayList<String>();

	/** Constructor.  §interface§ */
	public SVNRepositoryConnectorDescriptor() {
		this(null);
	}

	/** Constructor §interface§ */
	public SVNRepositoryConnectorDescriptor(AccountCredentials credentials) {
		super("Subversion", "http://subversion.apache.org/",
				"Reads files from a Subversion (SVN) repository. "
						+ "Supports file, https, svn, and ssh+svn protocols.",
				credentials, null);
		autoExpose();
	}

	/** Constructor §interface§ */
	public SVNRepositoryConnectorDescriptor(AccountCredentials credentials,
			int startRevision, int endRevision) {
		this(credentials);
		this.startRevision = startRevision;
		this.endRevision = endRevision;
	}

	/** {@inheritDoc} §interface§ */
	@Override
	protected String getChangeRetrieverBlockName() {
		return "org.conqat.engine.index.svn.SvnChangeRetriever";
	}

	/** {@inheritDoc}  §interface§ */
	@Override
	protected String getContentUpdaterBlockName() {
		return "org.conqat.engine.index.svn.SvnContentUpdater";
	}

	/** {@inheritDoc}  §interface§ */
	@Override
	protected void setCommonParameters(TriggerBuilder trigger)
			throws ProjectConfigurationException {

		trigger.addBlockParameter("repository.url", url);
		trigger.addBlockParameter("repository.username", username);
		trigger.addBlockParameter("repository.password", password);

		trigger.addBlockParameters("path.value", explicitPaths);
		trigger.addBlockParameters("excluded-directory.name",
				excludedDirectories);

		super.setCommonParameters(trigger);
	}

	/** {@inheritDoc} §interface§ */
	@Override
	public void validate() throws ConnectorValidationException {
		super.validate();

		try {
			SVNRepository repository = SVNUtils.createRepository(url, username,
					password);
			repository.testConnection();
			long lastRevision = endRevision;
			if (endRevision == 0) {
				lastRevision = repository.getLatestRevision();
			}

			long results = repository.log(null, startRevision, lastRevision,
					false, false, 1, null);
			if (results == 0) {
				throw new ConnectorValidationException(
						"Could not retrieve any revisions for the given path: "
								+ url + " in the revision range "
								+ startRevision + "-" + lastRevision);
			}
			// If startrevision == 0, don't check, we'll figure the correct   §inline§
			// starting revision out when we start reading the repo
			if (startRevision != 0) {
				// Check if for the start revision the path exists  §inline§
				repository.log(null, startRevision, startRevision, false,
						false, 1, null);
			}
		} catch (SVNException e) {
			handleSVNException(e);
		} catch (NumberFormatException | ProjectConfigurationException e) {
			throw new ConnectorValidationException(e.getMessage());
		}
	}

	/**
	 * Handles an {@link SVNException} by throwing a suitable   §interface§
	 * {@link ConnectorValidationException} depending on the error code.
	 */
	private void handleSVNException(SVNException e)
			throws ConnectorValidationException {
		SVNErrorCode errorCode = e.getErrorMessage().getErrorCode();
		if (errorCode == SVNErrorCode.FS_NOT_FOUND) {
			throw new ConnectorValidationException("Path '" + url
					+ "' does not exist for the startrevision " + startRevision
					+ " or endrevision " + endRevision);
		} else if (errorCode == SVNErrorCode.RA_SVN_IO_ERROR) {
			throw new ConnectorValidationException("Unknown host: " + url);
		} else if (errorCode == SVNErrorCode.RA_LOCAL_REPOS_OPEN_FAILED) {
			throw new ConnectorValidationException(
					"Could not open local repository at: " + url
							+ " (Path correct?)");
		} else if (errorCode == SVNErrorCode.RA_DAV_REQUEST_FAILED) {
			throw new ConnectorValidationException("No repository found at: "
					+ url + " (URL correct?)");
		}
		throw new ConnectorValidationException(e);
	}
}
