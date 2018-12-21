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
package org.conqat.engine.sourcecode.analysis;

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.analysis.shallowparsed.ShallowParsedFindingAnalyzerBase;
import org.conqat.engine.sourcecode.analysis.shallowparsed.ShallowParsingUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils;
import org.conqat.lib.scanner.ELanguage;

/**
 * Base class for detecting classes that implement hashCode() but not equals()
 * and vice versa. The name of the hashcode and equals methods have to be given
 * by the subclass, making the base class suitable for multiple languages.
 *
 * @author $Author: heinemann $
 * @version $Rev: 51193 $
 * @ConQAT.Rating GREEN Hash: 30F964E05077808185403A5BB68A488D
 */
public abstract class HashCodeAndEqualsAnalyzerBase extends
		ShallowParsedFindingAnalyzerBase {

	/** Returns the name of the method implementing the hashCode functionality */
	protected abstract String getHashCodeMethodName();

	/** Returns the name of the method implementing the equals functionality */
	protected abstract String getEqualsMethodName();

	/** Returns the languages this analysis runs on */
	protected abstract ELanguage getLanguage();

	/** {@inheritDoc} */
	@Override
	protected void analyzeElement(ITokenElement element) throws ConQATException {
		// ignore elements that use an unsupported language
		if (element.getLanguage() != getLanguage()) {
			return;
		}

		super.analyzeElement(element);
	}

	/** {@inheritDoc} */
	@Override
	protected void analyzeShallowEntities(ITokenElement element,
			List<ShallowEntity> entities) throws ConQATException {
		List<ShallowEntity> types = ShallowEntityTraversalUtils
				.listEntitiesOfType(entities, EShallowEntityType.TYPE);
		for (ShallowEntity type : types) {
			inspectType(element, type);
		}
	}

	/** Inspects the given type for the equals() and hashCode() methods. */
	private void inspectType(ITokenElement element, ShallowEntity type)
			throws ConQATException {
		List<ShallowEntity> methods = type
				.getChildrenOfType(EShallowEntityType.METHOD);

		ShallowEntity equalsEntity = null;
		ShallowEntity hashCodeEntity = null;
		for (ShallowEntity method : methods) {
			if (method.getName() == null) {
				continue;
			}
			if (method.getName().equals(getHashCodeMethodName())
					&& ShallowParsingUtils.extractParameterNameTokens(method)
							.size() == 0) {
				hashCodeEntity = method;
			} else if (method.getName().equals(getEqualsMethodName())
					&& ShallowParsingUtils.extractParameterNameTokens(method)
							.size() == 1) {
				equalsEntity = method;
			}
		}

		if (equalsEntity != null && hashCodeEntity == null) {
			createFindingForEntityStart(getEqualsMethodName() + "() but not "
					+ getHashCodeMethodName() + "() implemented in class "
					+ type.getName(), element, equalsEntity);
		} else if (equalsEntity == null && hashCodeEntity != null) {
			createFindingForEntityStart(
					getHashCodeMethodName() + "() but not "
							+ getEqualsMethodName()
							+ "() implemented in class " + type.getName(),
					element, hashCodeEntity);
		}
	}
}
