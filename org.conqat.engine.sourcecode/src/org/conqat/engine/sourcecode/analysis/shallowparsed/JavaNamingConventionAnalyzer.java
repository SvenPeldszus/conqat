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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49581 $
 * @ConQAT.Rating GREEN Hash: 8F51A35055A1F0959F73DAA34C6DA365
 */
@AConQATProcessor(description = "Configurable naming convention check for java. Identifiers that violate the conventions are marked with findings.")
public class JavaNamingConventionAnalyzer extends NamingConventionAnalyzer {

	/** {@inheritDoc} */
	@Override
	protected void checkAttribute(ITokenElement element, ShallowEntity entity)
			throws ConQATException {
		if (ShallowParsingUtils.isConstant(entity)
				&& entity.getName().equals("serialVersionUID")) {
			// Ignore this element
			return;
		}
		if (entity.getSubtype().equals("enum literal")) {
			checkName(element, entity, globalVariablePattern);
		} else {
			super.checkAttribute(element, entity);
		}
	}

	/** {@inheritDoc} */
	@Override
	protected void checkMethod(ITokenElement element, ShallowEntity entity)
			throws ConQATException {
		// static and non-static initializer
		if (entity.getSubtype().endsWith("static initializer")) {
			return; // nothing to check here
		}
		super.checkMethod(element, entity);
	}

	/** {@inheritDoc} */
	@Override
	protected void checkMeta(ITokenElement element, ShallowEntity entity)
			throws ConQATException {
		if (entity.getSubtype().equalsIgnoreCase(ETokenType.PACKAGE.name())) {
			checkName(element, entity, extractJavaPackageName(entity),
					modulePattern);
		}
	}

	/** Extracts the Java package name from a package meta entity. */
	private String extractJavaPackageName(ShallowEntity entity) {
		StringBuilder packageNameBuilder = new StringBuilder();
		for (IToken token : entity.includedTokens()) {
			if (token.getType() == ETokenType.IDENTIFIER) {
				if (packageNameBuilder.length() > 0) {
					packageNameBuilder.append(".");
				}
				packageNameBuilder.append(token.getText());
			}
		}
		return packageNameBuilder.toString();
	}

}
