/*-------------------------------------------------------------------------+
|                                                                          |
| Copyright 2005-2011 The ConQAT Project                                   |
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
package org.conqat.engine.sourcecode.resource;

import java.util.EnumSet;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.base.UniformPathHierarchyResourceSelectorBase;
import org.conqat.lib.scanner.ELanguage;

/**
 * {@ConQAT.Doc}
 * 
 * @author $Author: heinemann $
 * @version $Rev: 47743 $
 * @ConQAT.Rating GREEN Hash: A32444841A06441A593BC7C395DB3328
 */
@AConQATProcessor(description = "This processor selects all token resources. "
		+ "The selection may be limited to certain languages.")
public class TokenResourceSelector
		extends
		UniformPathHierarchyResourceSelectorBase<ITokenResource, TokenContainer> {

	/** The languages to include. */
	private final EnumSet<ELanguage> includedLanguages = EnumSet
			.noneOf(ELanguage.class);

	/** The languages to exclude. */
	private final EnumSet<ELanguage> excludedLanguages = EnumSet
			.noneOf(ELanguage.class);

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "language", description = ""
			+ "Adds languages to be included. If not set all languages are included.")
	public void addIncludedLanguage(
			@AConQATAttribute(name = "name", description = "Name of the language") ELanguage language) {
		includedLanguages.add(language);
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "exclude-language", description = ""
			+ "Adds languages to be excluded.")
	public void addExcludedLanguage(
			@AConQATAttribute(name = "name", description = "Name of the language") ELanguage language) {
		excludedLanguages.add(language);
	}

	/** {@inheritDoc} */
	@Override
	protected TokenContainer createRawContainer(String name) {
		return new TokenContainer(name);
	}

	/** {@inheritDoc} */
	@Override
	protected boolean keepElement(IResource element) {
		if (!(element instanceof ITokenElement)) {
			return false;
		}

		ELanguage language = ((ITokenElement) element).getLanguage();
		if (excludedLanguages.contains(language)) {
			return false;
		}

		if (includedLanguages.isEmpty()) {
			return true;
		}

		return includedLanguages.contains(language);
	}
}