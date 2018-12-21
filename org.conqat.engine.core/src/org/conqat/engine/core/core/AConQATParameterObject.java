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
package org.conqat.engine.core.core;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.conqat.lib.commons.string.StringUtils;

/**
 * Annotation for a member object containing parameters that are exposed to the
 * configuration. The annotated object may hold any type of parameters,
 * including again {@link AConQATParameterObject}s.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48556 $
 * @ConQAT.Rating GREEN Hash: 8B577C459A66DDF6A17F15E84F51B279
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface AConQATParameterObject {

	/**
	 * Prefix that will be prepended to all parameter names contained in the
	 * object. Default is not prepend a prefix.
	 */
	String namePrefix() default StringUtils.EMPTY_STRING;

	/**
	 * Prefix that will be prepended to all parameters descriptions contained in
	 * the object. Default is not prepend a description prefix.
	 */
	String descriptionPrefix() default StringUtils.EMPTY_STRING;
}