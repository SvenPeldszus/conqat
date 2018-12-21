/*-----------------------------------------------------------------------+
 | com.teamscale.index
 |                                                                       |
   $Id: TFSRepositoryConnection.java 49617 2014-06-26 13:47:00Z hummelb $            
 |                                                                       |
 | Copyright (c)  2009-2013 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package com.teamscale.index.repository.tfs;

import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.conqat.engine.commons.pattern.IncludeExcludePatternSupport;
import org.conqat.engine.index.repository.RepositoryChangeSet;
import org.conqat.engine.index.repository.RepositoryException;
import org.conqat.engine.index.repository.base.NumericRevisionRepositoryConnectionBase;
import org.conqat.engine.index.shared.ERepositoryChangeType;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.collections.CaseInsensitiveStringSet;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.string.StringUtils;

import com.microsoft.tfs.core.TFSTeamProjectCollection;
import com.microsoft.tfs.core.clients.versioncontrol.VersionControlClient;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.Change;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.Changeset;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.DeletedState;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.Item;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.ItemSet;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.ItemType;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.MergeSource;
import com.microsoft.tfs.core.clients.versioncontrol.soapextensions.RecursionType;
import com.microsoft.tfs.core.clients.versioncontrol.specs.ItemSpec;
import com.microsoft.tfs.core.clients.versioncontrol.specs.version.ChangesetVersionSpec;
import com.microsoft.tfs.core.clients.versioncontrol.specs.version.LatestVersionSpec;
import com.microsoft.tfs.core.exceptions.TECoreException;
import com.microsoft.tfs.core.httpclient.Credentials;
import com.microsoft.tfs.util.shutdown.ShutdownManager;

/**
 * This class describes the connection to a TFS version control repository. The
 * TFS API uses runtime exceptions to signal error conditions. As these are
 * poorly documented, we catch the most generic type of exception
 * {@link TECoreException} at the root of all public methods and convert them to
 * {@link RepositoryException}s.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49617 $
 * @ConQAT.Rating GREEN Hash: 83A1833673721642AAA375C600AB2FF2
 */
public class TFSRepositoryConnection extends
		NumericRevisionRepositoryConnectionBase {


	/** {@inheritDoc} */
	@Override
	public RepositoryChangeSet getChangeSet(String revision)
			throws RepositoryException {
		try {
			ChangesetVersionSpec version = new ChangesetVersionSpec(
					Integer.valueOf(revision));

			// We use queryHistory() instead of getChangeSet() below, because   §inline§
			// the former allows us to include our base path. We expect this to
			// return null (or an empty array) if the changeset does not include
			// files from our current base path. This way we avoid expensive
			// processing of commits that are not relevant to us.
			Changeset[] changeSets = vcClient.queryHistory(basePath, version,
					0, RecursionType.FULL, null, version, // from
					version, // to the same version, need exactly this one
					Integer.MAX_VALUE, true // include code changes
					, false, false, false // sort descending
					);

			if (changeSets == null || changeSets.length == 0) {
				return null;
			}

			CCSMAssert.isTrue(changeSets.length == 1,
					"Expected only one change set");

			return createChangeSet(revision, changeSets[0]);
		} catch (TECoreException e) {
			throw new RepositoryException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void close() {
		vcClient.close();
		tpc.close();
		synchronized (TFSRepositoryConnection.class) {
			openTfsConnections--;
			if (openTfsConnections == 0) {
				// This call is required explicitly to avoid caches from  §inline§
				// accumulating. See:
				// http://social.msdn.microsoft.com/Forums/vstudio/en-US/a20fbd0d-4cb0-481a-8aba-7f336d882f41/memory-leak-in-ms-tfs-java-sdk-10
				ShutdownManager.getInstance().shutdown();
			}
		}
	}
}
