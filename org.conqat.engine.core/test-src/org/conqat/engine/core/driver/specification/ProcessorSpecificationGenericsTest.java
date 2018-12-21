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
package org.conqat.engine.core.driver.specification;

import java.util.List;

import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.specification.processors.AbstractGenericProcessorInstance;
import org.conqat.engine.core.driver.specification.processors.GenericParameterObjectProcessorInstance;
import org.conqat.engine.core.driver.specification.processors.GenericProcessorBase;
import org.conqat.engine.core.driver.specification.processors.GenericProcessorInstance;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.lib.commons.reflect.ClassType;

/**
 * Tests for generic {@link ProcessorSpecification}s.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48563 $
 * @ConQAT.Rating GREEN Hash: 44F37772F1B6A2BBE0BCCBC44999D337
 */
public class ProcessorSpecificationGenericsTest extends
		ProcessorSpecificationTestBase {

	/** Test generic processor. */
	public void testGenericProcessor() {
		checkException(GenericProcessorBase.class,
				EDriverExceptionType.GENERIC_PROCESSOR_CLASS);
	}

	/** Test a processor having a generic return value. */
	public void testGenericReturnValue() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				GenericProcessorInstance.class.getName());

		assertEquals(new ClassType(Double.class),
				spec.getOutputs()[0].getType());
	}

	/**
	 * Test a processor having a generic return value in the context of an
	 * abstract base class. This documents bug #3271.
	 */
	public void testGenericReturnValueForAbstractBase() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				AbstractGenericProcessorInstance.class.getName());

		assertEquals(new ClassType(IConQATLogger.class),
				spec.getOutputs()[0].getType());
	}

	/** Test a processor having a generic method parameter. */
	public void testGenericMethodParameter() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				GenericProcessorInstance.class.getName());

		assertEquals(new ClassType(Double.class), spec.getParameter("param")
				.getAttributes()[0].getType());

		assertEquals(new ClassType(List.class), spec.getParameter("pls")
				.getAttributes()[0].getType());
		assertEquals(new ClassType(List.class), spec.getParameter("plq")
				.getAttributes()[0].getType());
		assertEquals(new ClassType(List.class), spec.getParameter("plx")
				.getAttributes()[0].getType());
	}

	/**
	 * Test a processor having a generic method parameter with an abstract base
	 * class.
	 */
	public void testGenericMethodParameterForAbstractBase() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				AbstractGenericProcessorInstance.class.getName());

		assertEquals(new ClassType(IConQATLogger.class),
				spec.getParameter("param").getAttributes()[0].getType());
	}

	/** Test a processor having a generic exposed field. */
	public void testGenericField() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				GenericProcessorInstance.class.getName());
		assertEquals(new ClassType(Double.class), spec.getParameter("field")
				.getAttributes()[0].getType());
	}

	/**
	 * Test a processor having a generic exposed field with an abstract base
	 * class.
	 */
	public void testGenericFieldForAbstractBase() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				AbstractGenericProcessorInstance.class.getName());
		assertEquals(new ClassType(IConQATLogger.class),
				spec.getParameter("field").getAttributes()[0].getType());
	}

	/** Test a processor having a generic exposed field in a parameter object. */
	public void testGenericParameterObjectField() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				GenericParameterObjectProcessorInstance.class.getName());
		assertEquals(new ClassType(Byte.class), spec
				.getParameter("bound-field").getAttributes()[0].getType());
		assertEquals(new ClassType(Integer.class),
				spec.getParameter("specified-field").getAttributes()[0]
						.getType());
		assertEquals(new ClassType(Double.class),
				spec.getParameter("inherited-field").getAttributes()[0]
						.getType());
		assertEquals(new ClassType(String.class),
				spec.getParameter("doubly-inherited-field").getAttributes()[0]
						.getType());
		assertEquals(new ClassType(Long.class),
				spec.getParameter("doubly-inherited-field2").getAttributes()[0]
						.getType());
		assertEquals(new ClassType(Short.class),
				spec.getParameter("nested-field").getAttributes()[0].getType());
		assertEquals(new ClassType(Double.class),
				spec.getParameter("enclosing-field").getAttributes()[0]
						.getType());
	}
}