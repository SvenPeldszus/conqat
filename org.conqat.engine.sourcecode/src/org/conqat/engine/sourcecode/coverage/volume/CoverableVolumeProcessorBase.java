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

import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.engine.sourcecode.resource.TokenElementProcessorBase;

/**
 * Base class for processors that calculate a coverable volume of a tokenized
 * resource.
 *
 * @author $Author: kinnen $
 * @version $Rev: 51097 $
 * @ConQAT.Rating GREEN Hash: 77C3FDB7C5F71B18F9972DD8BCC922F3
 */
public abstract class CoverableVolumeProcessorBase extends
		TokenElementProcessorBase {

	/** Key for the number of coverable entities. */
	@AConQATKey(description = "Number of coverable entitites.", type = "java.lang.Integer")
	public static final String COVERABLE_VOLUME_KEY = "coverable volume";

	/** Key for the list of coverable hints. */
	@AConQATKey(description = "Coverable entities", type = "java.util.List<org.conqat.engine.sourcecode.coverage.volume.LineHint>")
	public static final String COVERABLE_HINTS_KEY = "coverable hints";

	/** {@inheritDoc} */
	@Override
	protected void setUp(ITokenResource root) {
		NodeUtils.addToDisplayList(root, COVERABLE_VOLUME_KEY,
				COVERABLE_HINTS_KEY);
	}
}
