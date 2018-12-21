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
package org.conqat.engine.sourcecode.shallowparser;

import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * This class collects constants used for subtypes in a {@link ShallowEntity}.
 * The list of constants is not complete, as often the subtype is also taken
 * directly from the parsing context (e.g. a matched keyword). The purpose of
 * this class is only to eliminate redundancy between the parser and code
 * traversing the ASt.
 *
 * Convention is to have the name of the constant and the content the same
 * (while respecting naming conventions). Hence, changes to a value should be
 * reflected in the name.
 *
 * @author $Author: hummelb $
 * @version $Rev: 51180 $
 * @ConQAT.Rating GREEN Hash: 3AAFE4507033DF708D97F1AE8950EFAD
 */
public class SubTypeNames {

	/** Sub type. */
	public static final String ANNOTATION = "annotation";

	/** Sub type. */
	public static final String CLASS_PUBLICATION = "class publication";

	/** Sub type. */
	public static final String CLASS_DEFINITION = "class definition";

	/** Sub type. */
	public static final String CLASS_IMPLEMENTATION = "class implementation";

	/** Sub type. */
	public static final String INTERFACE_PUBLICATION = "interface publication";

	/** Sub type. */
	public static final String INTERFACE_DEFINITION = "interface definition";

	/** Sub type. */
	public static final String METHOD_IMPLEMENTATION = "method implementation";

	/** Sub type. */
	public static final String METHOD_DECLARATION = "method declaration";

	/** Sub type. */
	public static final String FUNCTION = "function";

	/** Sub type. */
	public static final String ON_CHANGE = "on change";

	/** Sub type. */
	public static final String NATIVE_SQL = "native SQL";

	/** Sub type. */
	public static final String SELECT_BLOCK = "select block";

	/** Sub type. */
	public static final String MODULE_INPUT = "module input";

	/** Sub type. */
	public static final String MODULE_OUTPUT = "module output";

	/** Sub type. */
	public static final String FORM = "form";

	/** Sub type. */
	public static final String SINGLE_SELECT = "single select";

	/** Sub type. */
	public static final String MACRO = "macro";

	/** Sub type. */
	public static final String VISIBILITY = "Visibility";

	/** Sub type. */
	public static final String EMPTY_STATEMENT = "empty statement";

	/** Sub type. */
	public static final String LOCAL_VARIABLE = "local variable";

	/** Sub type. */
	public static final String INTERFACE = "interface";

	/** Sub type. */
	public static final String IF = "if";

	/** Sub type. */
	public static final String ELSE_IF = "else if";

	/** Sub type. */
	public static final String ELSE_IF_NOSPACE = "elseif";

	/** Sub type. */
	public static final String DO = "do";

	/** Sub type. */
	public static final String FOR = "for";

	/** Sub type. */
	public static final String WHILE = "while";

	/** Sub type. */
	public static final String FOREACH = "foreach";

	/** Sub type. */
	public static final String ELSIF = "elsif";

	/** Sub type. */
	public static final String LOOP = "loop";

	/** Sub type. */
	public static final String CONTINUE = "continue";

	/** Sub type. */
	public static final String CASE = "case";

	/** Sub type. */
	public static final String SWITCH = "switch";

	/** Sub type. */
	public static final String DEFAULT = "default";

	/** Sub type. */
	public static final String WHEN = "when";

	/** Sub type. */
	public static final String ELSE = "else";

	/** Sub type. */
	public static final String FINALLY = "finally";

	/** Sub type. */
	public static final String TRY = "try";

	/** Sub type. */
	public static final String CATCH = "catch";

	/** Sub type. */
	public static final String EXIT = "exit";

	/** Sub type. */
	public static final String DECLARATION = "declaration";

	/** Sub type. */
	public static final String SIMPLE_STATEMENT = "simple statement";

	/** Sub type. */
	public static final String CLEANUP = "cleanup";

	/** Sub type. */
	public static final String PROVIDE = "provide";

	/** Sub type. */
	public static final String LABEL = "label";

	/** Sub type. */
	public static final String ANONYMOUS_BLOCK = "anonymous block";

	/** Sub type. */
	public static final String CHECKED = "checked";

	/** Sub type. */
	public static final String UNCHECKED = "unchecked";

	/** Sub type. */
	public static final String LOCK = "lock";

	/** Sub type. */
	public static final String USING = "using";

	/** Sub type. */
	public static final String FIXED = "fixed";

	/** Sub type. */
	public static final String YIELD = "yield";

	/** Sub type. */
	public static final String GOTO = "goto";

	/** Sub type. */
	public static final String LAMBDA_EXPRESSION = "lambda expression";

	/** Sub type. */
	public static final String SYNCHRONIZED = "synchronized";

	/** Sub type. */
	public static final String DATA = "data";

	/** Sub type. */
	public static final String STATICS = "statics";

	/** Sub type. */
	public static final String CONSTANTS = "constants";

	/** Sub type. */
	public static final String FIELD_SYMBOLS = "field-symbols";

	/** Sub type. */
	public static final String PARAMETERS = "parameters";

	/** Sub type. */
	public static final String TABLES = "tables";

	/** Sub type. */
	public static final String NODES = "nodes";

	/** Sub type. */
	public static final String SUBMIT = "submit";

	/** Sub type. */
	public static final String LEAVE = "leave";

	/** Sub type. */
	public static final String RETURN = "return";

	/** Sub type. */
	public static final String STOP = "stop";

	/** Sub type. */
	public static final String REJECT = "reject";

	/** Sub type. */
	public static final String RAISE = "raise";

	/** Sub type. */
	public static final String TYPES = "types";

	/** Sub type. */
	public static final String INCLUDE = "include";

	/** Sub type. */
	public static final String TYPE_POOLS = "type-pools";

	/** Sub type */
	public static final String STATIC_INITIALIZER = "static initializer";

}
