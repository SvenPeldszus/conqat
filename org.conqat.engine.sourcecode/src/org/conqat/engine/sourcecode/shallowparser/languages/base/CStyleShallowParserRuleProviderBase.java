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
package org.conqat.engine.sourcecode.shallowparser.languages.base;

import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowParserRuleProviderBase;

/**
 * Base class for rule providers for parsers with C-style syntax.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49567 $
 * @ConQAT.Rating GREEN Hash: 4E730B499A7C6D3430C3C64332F8C415
 */
public abstract class CStyleShallowParserRuleProviderBase<PARSER extends CStyleShallowParserBase>
		extends ShallowParserRuleProviderBase<EGenericParserStates, PARSER> {

	/** Constructor. */
	protected CStyleShallowParserRuleProviderBase(PARSER delegateParser) {
		super(delegateParser);
	}

	/**
	 * Creates a recognizer that matches all valid types, starting from the
	 * given state.
	 */
	protected RecognizerBase<EGenericParserStates> typePatternInState(
			EGenericParserStates... states) {
		return delegateParser.typePatternInState(states);
	}
}
