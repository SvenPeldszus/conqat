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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.IConQATParameterHolder;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.core.driver.error.DriverException;
import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.error.ErrorLocation;
import org.conqat.engine.core.driver.error.IErrorLocatable;
import org.conqat.engine.core.driver.error.ProcessorLayoutException;
import org.conqat.engine.core.driver.util.Multiplicity;
import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.reflect.GenericTypeResolver;
import org.conqat.lib.commons.reflect.ReflectionUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * A list of parameters parameter for a processor specification based on the
 * {@link ProcessorSpecificationParameter}s of a member field. This captures the
 * {@link AConQATParameterObject} annotation.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50577 $
 * @ConQAT.Rating GREEN Hash: 7863983ADEFE8193B11D12779393C0DF
 */
/* package */class ObjectBasedProcessorSpecificationParameter implements
		IErrorLocatable {

	/** String for separating parameter prefixes. */
	private static final String PARAMETER_PREFIX_SEPARATOR = "-";

	/** String for separating documentation description prefixes. */
	private static final String DESCRIPTION_PREFIX_SEPARATOR = StringUtils.SPACE;

	/** The annotation corresponding to the parameter object. */
	private final AConQATParameterObject annotation;

	/** The field being exposed. */
	private final Field field;

	/**
	 * The processor specification that hold the parameter object. In the case
	 * the parameter object is nested, this is the processor that holds the
	 * top-level parameter object.
	 */
	private final ProcessorSpecification specification;

	/** The generic type resolver of the processor specification. */
	private final GenericTypeResolver resolver;

	/**
	 * Creates a new parameter for a processor specification.
	 * 
	 * @param field
	 *            the field of the IConQATProcessor this parameter is
	 *            constructed from.
	 * @param resolver
	 *            the type resolver for the class to which the provided method
	 *            belongs to.
	 * @param specification
	 *            the specification this belongs to.
	 */
	ObjectBasedProcessorSpecificationParameter(Field field,
			GenericTypeResolver resolver, ProcessorSpecification specification)
			throws DriverException {
		this.field = field;
		this.resolver = resolver;
		this.specification = specification;
		this.annotation = field.getAnnotation(AConQATParameterObject.class);

		ModifierUtil.assertPublic(field, this);
		ModifierUtil.assertNotStatic(field, this);

		Class<?> objectClass = field.getType();
		if (!Modifier.isPublic(objectClass.getModifiers())) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.PARAMETER_OBJECT_CLASS_NOT_PUBLIC,
					"Parameter object class of field " + field.getName()
							+ " is not public!", this);
		}

		if (!IConQATParameterObject.class.isAssignableFrom(field.getType())) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.PARAMETER_OBJECT_CLASS_NOT_IMPLEMENTS_INTERFACE,
					"Parameter object class for field " + field.getName()
							+ "does not implement interface '"
							+ IConQATParameterObject.class.getName() + "'.",
					this);
		}
	}

	/** {@inheritDoc} */
	@Override
	public ErrorLocation getErrorLocation() {
		return specification.getErrorLocation();
	}

	/**
	 * returns all {@link ProcessorSpecificationParameter}s of the annotated
	 * object.
	 */
	private List<ProcessorSpecificationParameter> getParameters()
			throws DriverException {
		List<ProcessorSpecificationParameter> parameters = new ArrayList<>();
		for (ProcessorSpecificationParameter parameter : ProcessorSpecification
				.getParameters(field.getType(), specification, resolver)) {
			parameters.add(wrapParameter(parameter));
		}
		return parameters;
	}

	/**
	 * Returns all parameters that are encapsulated in fields annotated with
	 * {@link AConQATParameterObject} in the given class and its parent classes.
	 */
	/* package */static List<ProcessorSpecificationParameter> getParameters(
			Class<?> parameterHolder, ProcessorSpecification specification,
			GenericTypeResolver parentResolver) throws DriverException {
		List<ProcessorSpecificationParameter> parameters = new ArrayList<>();
		for (Field field : ReflectionUtils.getAllFields(parameterHolder)) {
			if (field.isAnnotationPresent(AConQATParameterObject.class)) {
				GenericTypeResolver resolver = new GenericTypeResolver(field,
						parentResolver);
				ObjectBasedProcessorSpecificationParameter parameterObject = new ObjectBasedProcessorSpecificationParameter(
						field, resolver, specification);
				parameters.addAll(parameterObject.getParameters());
			}
		}
		return parameters;
	}

	/**
	 * Wraps a {@link ProcessorSpecificationParameter} in a
	 * {@link NestedSpecificationParameter}.
	 */
	private NestedSpecificationParameter wrapParameter(
			ProcessorSpecificationParameter parameter) {
		return new NestedSpecificationParameter(parameter);
	}

	/**
	 * Nested parameter specification that proxies access through the
	 * {@link IConQATParameterObject}.
	 */
	private class NestedSpecificationParameter extends
			ProcessorSpecificationParameter {

		/** The nested parameter specification. */
		private final ProcessorSpecificationParameter nestedParameter;

		/** Constructor. */
		public NestedSpecificationParameter(
				ProcessorSpecificationParameter parameter) {
			super(StringUtils.addPrefix(parameter.getName(),
					PARAMETER_PREFIX_SEPARATOR, annotation.namePrefix()), parameter
					.getSpecification());
			nestedParameter = parameter;
		}

		/** {@inheritDoc} */
		@Override
		public void applyParameter(IConQATParameterHolder target,
				Object[] attributeValues) throws IllegalArgumentException,
				IllegalAccessException, InvocationTargetException {
			Object object = field.get(target);

			CCSMAssert.isNotNull(object,
					"Attempt to assign value to parameter '" + getName()
							+ "' of a parameter object that is null.");

			// Already checked in specification constructor.
			CCSMAssert.isInstanceOf(object, IConQATParameterHolder.class);

			nestedParameter.applyParameter((IConQATParameterHolder) object,
					attributeValues);
		}

		/** {@inheritDoc} */
		@Override
		public Multiplicity getMultiplicity() {
			return nestedParameter.getMultiplicity();
		}

		/** {@inheritDoc} */
		@Override
		public String getDoc() {
			return StringUtils.addPrefix(nestedParameter.getDoc(),
					DESCRIPTION_PREFIX_SEPARATOR, annotation.descriptionPrefix());
		}

		/** {@inheritDoc} */
		@Override
		public ProcessorSpecificationAttribute[] getAttributes() {
			return nestedParameter.getAttributes();
		}
	}
}