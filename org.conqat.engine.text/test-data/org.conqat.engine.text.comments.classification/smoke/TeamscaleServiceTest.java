/*-----------------------------------------------------------------------+
 | com.teamscale.index
 |                                                                       |
   $Id: TeamscaleServiceTest.java 49617 2014-06-26 13:47:00Z hummelb $            
 |                                                                       |
 | Copyright (c)  2009-2012 CQSE GmbH                                 |
 +-----------------------------------------------------------------------*/
package com.teamscale.index.systemtest;

import java.io.File;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.conqat.engine.commons.pattern.PatternTransformationList;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.index.core.trigger.TriggerCompilationException;
import org.conqat.engine.index.shared.TrackedFinding;
import org.conqat.engine.service.external.ExternalAnalysisResultsPublisher;
import org.conqat.engine.service.shared.client.ServiceCallException;
import org.conqat.engine.service.shared.data.FindingBlacklistInfo;
import org.conqat.engine.sourcecode.analysis.SourceCodeSearchFindingAnalyzer;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.string.RegexReplacement;

/**
 * System test for services located in the Teamscale index bundle.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49617 $
 * @ConQAT.Rating GREEN Hash: 7075EA46E1DDAA7EEFE17B5C029872C1
 */
public class TeamscaleServiceTest extends ServiceClientSystemTestBase {

	/** Tests the upload of external analysis results. */
	public void testExternalAnalysisResults() throws ConQATException,
			ServiceCallException, InterruptedException,
			TriggerCompilationException {
		// create findings for identifiers containing "new"
		ITokenResource tokenRoot = createJavaResourceHierarchyFor(useTestFile("ext-analysis"));

		// we 'reuse' existing category and group of the analysis profile to  §inline§
		// avoid adapting it
		String categoryOverride = "Comments";
		String groupOverride = "Empty Interface Comment";

		executeProcessor(
				SourceCodeSearchFindingAnalyzer.class,
				"(input=(ref=",
				tokenRoot,
				"), search=(patterns=patList('[nN]ew.*')), 'token-type'=(value=IDENTIFIER), 'finding-category'=(name='"
						+ categoryOverride
						+ "'), 'finding-group'=(name='"
						+ groupOverride + "'))");

		// upload these findings
		PatternTransformationList transformation = new PatternTransformationList();
		transformation.add(new RegexReplacement("^", "testsystem/src/test/"));
		executeProcessor(
				ExternalAnalysisResultsPublisher.class,
				"(input=(ref=",
				tokenRoot,
				"), project=(name='",
				TESTPROJECT,
				"'), analysis=(identifier=search), 'path-transformations'=(ref=",
				transformation, "), server=(url='" + serverDetails.getUrl()
						+ "', username='" + serverDetails.getPassword()
						+ "', password='" + serverDetails.getPassword() + "'))");

		// wait for one more analysis to complete
		waitForAnalysis(getNumExpectedLogEntries() + 1);

		assertSearchFinding(client.getFindingsRecursive("testsystem/",
				TESTPROJECT, serverDetails));
	}

}
