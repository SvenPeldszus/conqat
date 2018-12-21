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
package org.conqat.engine.sourcecode.coverage.volume;

import java.util.List;

import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.commons.filesystem.CanonicalFile;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.scanner.ELanguage;
import org.junit.Ignore;

/**
 * Testlet for {@link CoverableVolumeProcessorTest}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51548 $
 * @ConQAT.Rating GREEN Hash: 318F4EC746B10751360B5A307AEBA427
 */
@Ignore
public class CoverableVolumeProcessorTestlet extends TokenTestCaseBase {

	/** The file containing the source code. */
	private final CanonicalFile codeFile;

	/** The processor used to calculate the volume. */
	private final Class<? extends CoverableVolumeProcessorBase> coverageProcessor;

	/** The expected volume. */
	private final int expectedVolume;

	/** Constructor. */
	/* package */CoverableVolumeProcessorTestlet(CanonicalFile codeFile,
			Class<? extends CoverableVolumeProcessorBase> coverageProcessor,
			int expectedVolume) {
		setName("test");

		this.codeFile = codeFile;
		this.coverageProcessor = coverageProcessor;
		this.expectedVolume = expectedVolume;
	}

	/** The test method. */
	public void test() throws Exception {
		ELanguage language = ELanguage.fromFile(codeFile);
		assertNotNull("No language found for file " + codeFile, language);
		ITokenElement element = createTokenElement(codeFile, language);
		executeProcessor(coverageProcessor, "(input=(ref=", element, "))");

		int actualVolume = (int) element
				.getValue(CoverableVolumeProcessorBase.COVERABLE_VOLUME_KEY);
		if (expectedVolume != actualVolume) {
			String hints = StringUtils
					.concat((List<?>) element
							.getValue(CoverableVolumeProcessorBase.COVERABLE_HINTS_KEY),
							StringUtils.CR);
			fail("Expected " + expectedVolume + " but had " + actualVolume
					+ ". Hints: " + StringUtils.CR + hints);
		}
	}

	/** Name of the test case is the name of the smoke test file. */
	@Override
	public String getName() {
		return "[" + coverageProcessor.getSimpleName() + "] "
				+ codeFile.getName();
	}
}
