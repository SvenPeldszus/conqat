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
package org.conqat.engine.sourcecode.shallowparser.framework;

/**
 * Base class for rule extenders, i.e., classes that contribute parsing rules to
 * a shallow parser. This mechanism is used to split a parser into multiple
 * classes.
 * 
 * @author $Author: goede $
 * @version $Rev: 48325 $
 * @ConQAT.Rating GREEN Hash: F1756BEA55B61CA820CE659375E49FE1
 */
public abstract class ShallowParserRuleProviderBase<STATE extends Enum<STATE>, PARSER extends ShallowParserBase<STATE>> {

	/** The delegate used for rule creation. */
	protected final PARSER delegateParser;

	/** Constructor. */
	protected ShallowParserRuleProviderBase(PARSER delegateParser) {
		this.delegateParser = delegateParser;
	}

	/** Template method for contributing new rules to the {@link #delegateParser} */
	public abstract void contributeRules();

	/** Creates a rule that is active in each of the given states. */
	protected RecognizerBase<STATE> inState(
			@SuppressWarnings("unchecked") STATE... states) {
		return delegateParser.inState(states);
	}

	/** Returns an empty recognizer that can be used for local sub-rules. */
	protected RecognizerBase<STATE> emptyRecognizer() {
		return delegateParser.emptyRecognizer();
	}

	/** Creates a rule that is active in any state. */
	protected RecognizerBase<STATE> inAnyState() {
		return delegateParser.inAnyState();
	}
}
