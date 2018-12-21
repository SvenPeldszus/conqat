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

import java.util.HashSet;
import java.util.Set;

import org.conqat.engine.core.driver.error.DriverException;
import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.specification.processors.ProcessorForSpecTestWithPipeline;
import org.conqat.engine.core.driver.specification.processors.ProcessorToTestSpec;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithDuplicateParameterName;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithIncompatiblePipeline;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithoutAnnotation;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithoutInterface;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithoutParameterlessConstructor;
import org.conqat.engine.core.driver.specification.processors.ProcessorWithoutPublicConstructor;
import org.conqat.engine.core.driver.util.IDocumented;
import org.conqat.engine.core.driver.util.Multiplicity;
import org.conqat.lib.commons.reflect.ClassType;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Tests for {@link ProcessorSpecification}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48607 $
 * @ConQAT.Rating GREEN Hash: 243DD8DFF73AE2E1AC71066F9322F8E0
 */
public class ProcessorSpecificationTest extends ProcessorSpecificationTestBase {

	/**
	 * Tests whether all processor details (parameters, outputs, documentation,
	 * etc.) are read correctly from the class.
	 */
	public void testSpecificationDetails() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				ProcessorToTestSpec.class.getName());

		assertNameAndDoc(spec, ProcessorToTestSpec.class.getName(), "desc");

		checkProcessorOutput(spec);
		checkProcessorParamPresence(spec);
		checkProcessorMethodParams(spec);
		checkProcessorFieldParams(spec);
		checkProcessorObjectParams(spec);
	}

	/** Checks the validity of the processor output. */
	private void checkProcessorOutput(ProcessorSpecification spec) {
		assertEquals(1, spec.getOutputs().length);
		assertEquals(StringUtils.EMPTY_STRING, spec.getOutputs()[0].getName());
		assertEquals(new ClassType(Object.class),
				spec.getOutputs()[0].getType());
	}

	/**
	 * Checks the presence of all specified processor parameters including the
	 * conditional synthetic parameter.
	 */
	private void checkProcessorParamPresence(ProcessorSpecification spec) {
		assertEquals(6, spec.getParameters().length);
		assertTrue(spec.getParameters()[0].isSynthetic());
		assertEquals(IConditionalParameter.PARAMETER_NAME,
				spec.getParameters()[0].getName());

		Set<String> paramNames = new HashSet<String>();
		for (ISpecificationParameter param : spec.getNonSyntheticParameters()) {
			paramNames.add(param.getName());
		}

		assertTrue(paramNames.contains("xset"));
		assertTrue(paramNames.contains("mult"));
		assertTrue(paramNames.contains("field_p"));
		assertTrue(paramNames.contains("prefix-obj_p"));
		assertTrue(paramNames.contains("prefix-nested_obj_p"));
	}

	/** Checks the validity of method parameters of the processor. */
	private void checkProcessorMethodParams(ProcessorSpecification spec)
			throws DriverException {
		ISpecificationParameter xset = spec.getParameter("xset");
		assertNameAndDoc(xset, "xset", "xset_desc");
		assertMultiplicityAndNumAttributes(xset, 0, Multiplicity.INFINITY, 2);

		assertNameAndDoc(xset.getAttributes()[0], "at_a", "at_a_desc");
		assertNoPipelineOutputAndDefaultValue(xset.getAttributes()[0]);
		assertEquals(new ClassType(int.class),
				xset.getAttributes()[0].getType());

		assertNameAndDoc(xset.getAttributes()[1], "at_b", "at_b_desc");
		assertFalse(xset.getAttributes()[1].hasPipelineOutputs());
		assertEquals("test", xset.getAttributes()[1].getDefaultValue());
		assertEquals(new ClassType(String.class),
				xset.getAttributes()[1].getType());

		ISpecificationParameter mult = spec.getParameter("mult");
		assertNameAndDoc(mult, "mult", "mult_desc");
		assertMultiplicityAndNumAttributes(mult, 7, 42, 0);
	}

	/** Checks the validity of field parameters of the processor. */
	private void checkProcessorFieldParams(ProcessorSpecification spec)
			throws DriverException {
		ISpecificationParameter field = spec.getParameter("field_p");
		assertNameAndDoc(field, "field_p", "field_d");
		assertMultiplicityAndNumAttributes(field, 0, 1, 1);

		SpecificationAttribute fieldAttr = field.getAttributes()[0];
		assertNameAndDoc(fieldAttr, "field_a", "field_d");
		assertNoPipelineOutputAndDefaultValue(fieldAttr);
		assertEquals(new ClassType(double.class), fieldAttr.getType());
	}

	/** Checks the validity of the object parameters of the processor. */
	private void checkProcessorObjectParams(ProcessorSpecification spec)
			throws DriverException {
		ISpecificationParameter obj = spec.getParameter("prefix-obj_p");
		assertNameAndDoc(obj, "prefix-obj_p", "descr_prefix obj_d");
		assertMultiplicityAndNumAttributes(obj, 0, 1, 1);

		SpecificationAttribute objAttr = obj.getAttributes()[0];
		assertNameAndDoc(objAttr, "obj_a", "obj_d");
		assertNoPipelineOutputAndDefaultValue(objAttr);
		assertEquals(new ClassType(String.class), objAttr.getType());

		ISpecificationParameter nestedObj = spec
				.getParameter("prefix-nested_obj_p");
		assertNameAndDoc(nestedObj, "prefix-nested_obj_p",
				"descr_prefix nested_obj_d");
		assertMultiplicityAndNumAttributes(nestedObj, 0, 1, 1);

		SpecificationAttribute nestedObjAttr = nestedObj.getAttributes()[0];
		assertNameAndDoc(nestedObjAttr, "nested_obj_a", "nested_obj_d");
		assertNoPipelineOutputAndDefaultValue(nestedObjAttr);
		assertEquals(new ClassType(int.class), nestedObjAttr.getType());
	}

	/**
	 * Asserts that the specification has the given name and documentation
	 * string.
	 */
	private static void assertNameAndDoc(IDocumented spec, String name,
			String doc) {
		assertEquals(name, spec.getName());
		assertEquals(doc, spec.getDoc());
	}

	/**
	 * Asserts that the parameter has the given multiplicity and number of
	 * attributes.
	 */
	private static void assertMultiplicityAndNumAttributes(
			ISpecificationParameter param, int multLower, int multUpper,
			int numAttributes) {
		assertEquals(new Multiplicity(multLower, multUpper),
				param.getMultiplicity());
		assertEquals(numAttributes, param.getAttributes().length);
	}

	/** Asserts that the attribute has no pipeline outputs and no default value. */
	private static void assertNoPipelineOutputAndDefaultValue(
			SpecificationAttribute attrib) throws DriverException {
		assertFalse(attrib.hasPipelineOutputs());
		assertNull(attrib.getDefaultValue());
	}

	/** Test whether pipeline information is extracted correctly. */
	public void testWithPipeline() throws Exception {
		ProcessorSpecification spec = new ProcessorSpecification(
				ProcessorForSpecTestWithPipeline.class.getName());

		assertEquals(new ClassType(Integer.class),
				spec.getOutputs()[0].getType());

		// must be 2: our parameter and the synthetic condition parameter
		assertEquals(2, spec.getParameters().length);
		assertTrue(spec.getParameters()[1].getName().equals("xset"));

		ISpecificationParameter xset = spec.getParameter("xset");
		assertEquals(2, xset.getAttributes().length);

		assertEquals(1, xset.getAttributes()[0].getPipelineOutputs().size());
		assertEquals(spec.getOutputs()[0], xset.getAttributes()[0]
				.getPipelineOutputs().get(0));

		assertEquals(0, xset.getAttributes()[1].getPipelineOutputs().size());
	}

	/** Test processor not implementing <code>IConQATProcessor</code> */
	public void testProcessorNotImplementingIConQATProcessor() {
		checkException(ProcessorWithoutInterface.class,
				EDriverExceptionType.PROCESSOR_CLASS_NOT_IMPLEMENTS_INTERFACE);
	}

	/** Test an unknown processor class. */
	public void testUnknownProcessorClass() {
		checkException("some.class.which.does.not.exists",
				EDriverExceptionType.PROCESSOR_CLASS_NOT_FOUND);
	}

	/** Test processor without annotation. */
	public void testProcessorWithoutAnnotation() {
		checkException(ProcessorWithoutAnnotation.class,
				EDriverExceptionType.PROCESSOR_CLASS_NOT_ANNOTATED);
	}

	/** Test processor without parameterless constructor. */
	public void testProcessorWithoutParameterlessConstructor() {
		checkException(
				ProcessorWithoutParameterlessConstructor.class,
				EDriverExceptionType.PROCESSOR_CLASS_WITHOUT_PARAMETERLESS_CONSTRUCTOR);
	}

	/** Test processor without public constructor. */
	public void testProcessorWithoutPublicConstructor() {
		checkException(ProcessorWithoutPublicConstructor.class,
				EDriverExceptionType.PROCESSOR_CLASS_WITHOUT_PUBLIC_CONSTRUCTOR);
	}

	/** Test processor with different types in the pipeline. */
	public void testProcessorWithTypeConflictInPipeline() {
		checkException(ProcessorWithIncompatiblePipeline.class,
				EDriverExceptionType.INCOMPATIBLE_PIPELINE_TYPES);
	}

	/** Test processor with duplicate name for parameter. */
	public void testProcessorWithDuplicateParameterName() {
		checkException(ProcessorWithDuplicateParameterName.class,
				EDriverExceptionType.DUPLICATE_PARAM_NAME);
	}
}