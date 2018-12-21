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

/**
 * Enumeration of parser states that are rather generic and thus applicable for
 * many of the shallow parsers.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49539 $
 * @ConQAT.Rating GREEN Hash: EAB7ED7F996FE59B4A2557436D59D33E
 */
public enum EGenericParserStates {

	/** Top level state. */
	TOP_LEVEL,

	/** Currently within module definition (or package, namespace, etc.). */
	IN_MODULE,

	/** Currently within type definition (class, etc.). */
	IN_TYPE,

	/** Currently within method/procedure/function definition. */
	IN_METHOD,

	/**
	 * Currently within an expression. Typically, we expect anonymous classes,
	 * lambdas, etc. in this state.
	 */
	IN_EXPRESSION;

}
