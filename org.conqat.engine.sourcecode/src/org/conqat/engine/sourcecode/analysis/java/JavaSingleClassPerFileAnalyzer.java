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
package org.conqat.engine.sourcecode.analysis.java;

import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47763 $
 * @ConQAT.Rating GREEN Hash: A9D64F2E3F4269A03087A516CEB7AC10
 */
@AConQATProcessor(description = "Produces a finding if a file contains more than one top-level class.")
public class JavaSingleClassPerFileAnalyzer extends JavaFindingAnalyzerBase {

	/** The name of the group to place the findings in. */
	public static final String FINDINGS_GROUP_NAME = "More than one top-level class";

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {
		boolean hadClass = false;
		for (ShallowEntity entity : entities) {
			if (entity.getType() != EShallowEntityType.TYPE) {
				continue;
			}

			if (hadClass) {
				createFindingForEntityStart(
						"More than one top-level class in file!", element,
						entity);
				return;
			}
			hadClass = true;
		}
	}

	/** {@inheritDoc} */
	@Override
	protected String getFindingGroupName() {
		return FINDINGS_GROUP_NAME;
	}
}
