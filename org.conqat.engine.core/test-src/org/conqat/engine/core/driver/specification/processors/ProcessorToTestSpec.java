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
package org.conqat.engine.core.driver.specification.processors;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.IConQATParameterObject;

/**
 * Processor for testing purposes.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49408 $
 * @ConQAT.Rating GREEN Hash: F9D2BEE2ABF6DC07B9A9A077DC28EB4D
 */
@SuppressWarnings("unused")
@AConQATProcessor(description = "desc")
public class ProcessorToTestSpec extends TestProcessorBase {

	/** Test field */
	@AConQATFieldParameter(parameter = "field_p", attribute = "field_a", optional = true, description = "field_d")
	public double field;

	/** Test method. */
	@AConQATParameter(description = "xset_desc", name = "xset")
	public void setX(
			@AConQATAttribute(description = "at_a_desc", name = "at_a") int a,
			@AConQATAttribute(description = "at_b_desc", name = "at_b", defaultValue = "test") String b) {
		// nothing to do here
	}

	/** Test method. */
	@AConQATParameter(description = "mult_desc", name = "mult", minOccurrences = 7, maxOccurrences = 42)
	public void mult() {
		// nothing to do here
	}

	/** Test parameter object. */
	@AConQATParameterObject(namePrefix = "prefix", descriptionPrefix = "descr_prefix")
	public ParameterObject definition = new ParameterObject();

	/** Object encapsulating a parameter. */
	public class ParameterObject implements IConQATParameterObject {

		/** Test field */
		@AConQATFieldParameter(parameter = "obj_p", attribute = "obj_a", optional = true, description = "obj_d")
		public String object;

		/** Test parameter object */
		@AConQATParameterObject
		public NestedParameterObject nested = new NestedParameterObject();

	}

	/** Nested object encapsulating a parameter. */
	public class NestedParameterObject implements IConQATParameterObject {

		/** Test field */
		@AConQATFieldParameter(parameter = "nested_obj_p", attribute = "nested_obj_a", optional = true, description = "nested_obj_d")
		public int nestedObject;

	}

}