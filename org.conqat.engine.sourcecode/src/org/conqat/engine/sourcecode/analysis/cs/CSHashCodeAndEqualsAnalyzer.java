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
package org.conqat.engine.sourcecode.analysis.cs;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.sourcecode.analysis.HashCodeAndEqualsAnalyzerBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 *
 * @author $Author: hummelb $
 * @version $Rev: 50499 $
 * @ConQAT.Rating GREEN Hash: 6A857291E23A0323F6B60F733F0503A5
 */
@AConQATProcessor(description = "This processor reports classes that implement only one of GetHashCode() or Equals(), but not both.")
public class CSHashCodeAndEqualsAnalyzer extends HashCodeAndEqualsAnalyzerBase {

	/** The name to use for the findings group. */
	public static final String FINDINGS_GROUP_NAME = "GetHashCode and Equals";

	/** Name of the findings category. */
	public static final String FINDINGS_CATEGORY_NAME = "CS Checks";

	/** {@inheritDoc} */
	@Override
	protected ELanguage getLanguage() {
		return ELanguage.CS;
	}

	/** {@inheritDoc} */
	@Override
	protected String getEqualsMethodName() {
		return "Equals";
	}

	/** {@inheritDoc} */
	@Override
	protected String getHashCodeMethodName() {
		return "GetHashCode";
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingCategoryName() {
		return FINDINGS_CATEGORY_NAME;
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}
}
