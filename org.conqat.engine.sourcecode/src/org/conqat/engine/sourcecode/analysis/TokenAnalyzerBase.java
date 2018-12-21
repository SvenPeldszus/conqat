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
package org.conqat.engine.sourcecode.analysis;

import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.analysis.ElementAnalyzerBase;
import org.conqat.engine.resource.regions.RegionSetDictionary;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.region.RegionSet;
import org.conqat.lib.scanner.IToken;

/**
 * Base class to analyze the token sequence. Performs additional filtering if a
 * {@link RegionSetDictionary} is stored in the filter keys.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49014 $
 * @ConQAT.Rating GREEN Hash: 2A0C8395159B4858BAF3C59C6469863C
 */
public abstract class TokenAnalyzerBase extends
		ElementAnalyzerBase<ITokenResource, ITokenElement> {

	/** {@inheritDoc} */
	@Override
	protected void analyzeElement(ITokenElement element) throws ConQATException {

		List<IToken> tokens;
		try {
			tokens = element.getTokens(getLogger());
		} catch (ConQATException e) {
			getLogger().warn("Problems scanning element: ", e);
			return;
		}
		tokens = filter(tokens, element);

		analyzeTokens(tokens, element);
	}

	/** Filters ignored tokens. Does not modify the passed tokens list. */
	private List<IToken> filter(List<IToken> tokens, ITokenElement element)
			throws ConQATException {
		List<IToken> filtered = new ArrayList<IToken>(tokens);

		for (String filterKey : getFilterKeys()) {
			filterTokens(filtered, element, filterKey);
		}

		return filtered;
	}

	/**
	 * Filters ignored tokens for a single filter key. Modifies the passed token
	 * list.
	 */
	private void filterTokens(List<IToken> tokens, ITokenElement element,
			String filterKey) throws ConQATException {
		RegionSet regionSet = RegionSetDictionary.retrieve(element, filterKey);
		if (regionSet == null) {
			return;
		}

		for (IToken token : new ArrayList<IToken>(tokens)) {
			if (regionSet.contains(token.getOffset())) {
				tokens.remove(token);
			}
		}
	}

	/** Analyze the sequence of tokens of which an element consists */
	protected abstract void analyzeTokens(List<IToken> tokens,
			ITokenElement element) throws ConQATException;
}