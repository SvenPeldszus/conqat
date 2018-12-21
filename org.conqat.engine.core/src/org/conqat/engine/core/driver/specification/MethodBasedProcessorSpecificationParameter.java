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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.IConQATParameterHolder;
import org.conqat.engine.core.driver.error.BlockFileException;
import org.conqat.engine.core.driver.error.DriverException;
import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.error.ProcessorLayoutException;
import org.conqat.engine.core.driver.util.Multiplicity;
import org.conqat.lib.commons.reflect.FormalParameter;
import org.conqat.lib.commons.reflect.GenericTypeResolver;
import org.conqat.lib.commons.reflect.ReflectionUtils;

/**
 * A parameter for a processor specification based on a method. This captures
 * the {@link AConQATParameter} annotation.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49465 $
 * @ConQAT.Rating GREEN Hash: 2CBD5BADBB902995E42D8F346DCE6597
 */
/* package */class MethodBasedProcessorSpecificationParameter extends
		ProcessorSpecificationParameter {

	/** The annotation corresponding to this parameter. */
	private final AConQATParameter annotation;

	/**
	 * The method to be called to apply this parameter to the actual instance of
	 * the IConQATProcessor.
	 */
	private final Method method;

	/**
	 * Creates a new parameter for a processor specification.
	 * 
	 * @param method
	 *            the method of the IConQATProcessor this parameter is
	 *            constructed from.
	 * @param resolver
	 *            the type resolver for the class to which the provided method
	 *            belongs to.
	 * @param specification
	 *            the specification this belongs to.
	 */
	/* package */MethodBasedProcessorSpecificationParameter(Method method,
			GenericTypeResolver resolver, ProcessorSpecification specification)
			throws DriverException {
		super(method.getAnnotation(AConQATParameter.class).name(),
				specification);

		annotation = method.getAnnotation(AConQATParameter.class);
		this.method = method;

		ModifierUtil.assertPublic(method, this);
		ModifierUtil.assertNotStatic(method, this);

		checkAllFormalParametersAnnotated(method, resolver);
		checkConsistencyOfMultiplicity();
	}

	/**
	 * Ensures that all formal parameters of the method have been annotated as
	 * ConQAT attributes
	 */
	private void checkAllFormalParametersAnnotated(Method method,
			GenericTypeResolver resolver) throws ProcessorLayoutException,
			BlockFileException, DriverException {

		for (FormalParameter formalParameter : ReflectionUtils
				.getFormalParameters(method)) {

			if (!formalParameter.isAnnotationPresent(AConQATAttribute.class)) {
				throw new ProcessorLayoutException(
						EDriverExceptionType.FORMAL_PARAMETER_NOT_ANNOTATED,
						"Formal parameter at position "
								+ formalParameter.getPosition() + " in method "
								+ method.getName() + " is not annotated with '"
								+ AConQATAttribute.class.getSimpleName() + "'.",
						this);
			}
			addAttribute(new MethodBasedProcessorSpecificationAttribute(
					formalParameter, resolver, this));
		}
	}

	/**
	 * Ensures that the multiplicity of this parameter, as read from the
	 * annotations, is valid: Intervals must not be empty and pipeline
	 * parameters must have a multiplicity of 1.
	 */
	private void checkConsistencyOfMultiplicity()
			throws ProcessorLayoutException {
		if (getMultiplicity().isEmpty()) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.EMPTY_PARAMETER_INTERVAL, this
							+ " defines an empty multiplicity interval.", this);
		}
	}

	/** {@inheritDoc} */
	@Override
	public Multiplicity getMultiplicity() {
		int upper = annotation.maxOccurrences();
		if (upper < 0) {
			upper = Multiplicity.INFINITY;
		}
		return new Multiplicity(annotation.minOccurrences(), upper);
	}

	/** {@inheritDoc} */
	@Override
	public void applyParameter(IConQATParameterHolder target,
			Object[] attributeValues) throws IllegalArgumentException,
			IllegalAccessException, InvocationTargetException {
		method.invoke(target, attributeValues);
	}

	/** {@inheritDoc} */
	@Override
	public String getDoc() {
		return annotation.description();
	}

	/**
	 * Returns all parameters that are specified by methods annotated with
	 * {@link AConQATParameter} in the given class and its parent classes.
	 */
	public static List<ProcessorSpecificationParameter> getParameters(
			Class<?> parameterHolder, ProcessorSpecification specification,
			GenericTypeResolver resolver) throws DriverException {
		List<ProcessorSpecificationParameter> parameters = new ArrayList<>();

		for (Method method : ReflectionUtils.getAllMethods(parameterHolder)) {
			if (method.isAnnotationPresent(AConQATParameter.class)) {
				parameters.add(new MethodBasedProcessorSpecificationParameter(
						method, resolver, specification));
			}
		}
		return parameters;
	}
}
