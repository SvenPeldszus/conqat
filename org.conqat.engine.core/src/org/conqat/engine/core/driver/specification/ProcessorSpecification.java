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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.AThreadSafeProcessor;
import org.conqat.engine.core.core.IConQATProcessor;
import org.conqat.engine.core.driver.error.BlockFileException;
import org.conqat.engine.core.driver.error.DriverException;
import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.error.EnvironmentException;
import org.conqat.engine.core.driver.error.ErrorLocation;
import org.conqat.engine.core.driver.error.ProcessorLayoutException;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.reflect.GenericTypeResolver;

/**
 * This is the specification of a processor. The detailed informations are taken
 * from the annotations of the processor, which is what most of the code deals
 * with (during compilation).
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49407 $
 * @ConQAT.Rating GREEN Hash: F32988A8420E91B5347219DA4B8E5FBA
 */
public class ProcessorSpecification extends
		SpecificationBase<ProcessorSpecificationParameter> {

	/** The name of the process method ({@link IConQATProcessor#process()}). */
	private static final String PROCESSOR_WORKER_METHOD = "process";

	/** The class of the underlying processor. */
	private Class<?> processorClass = null;

	/** The annotation of the processor used for documentation */
	private AConQATProcessor annotation;

	/**
	 * Whether this processor is thread safe (as indicated by
	 * {@link AThreadSafeProcessor}.
	 */
	private boolean threadSafe;

	/**
	 * The only (default) output of a processor. As the type of output is
	 * determined during compilation, we initialize with null here.
	 */
	private ProcessorSpecificationOutput defaultOutput = null;

	/** The list of keys for this processor. */
	private final List<KeySpecification> keys = new ArrayList<KeySpecification>();

	/**
	 * Create a new processor specification.
	 * 
	 * @param name
	 *            the name of this specification (which corresponds to the class
	 *            name of the implementing processor class).
	 */
	/* package */ProcessorSpecification(String name) throws DriverException {
		super(name);
		init();
	}

	/** Initialize this specification using reflection on the processor class. */
	private void init() throws DriverException {
		determineClass();

		// test if creation of processor is possible
		createProcessorInstance();

		// prepare resolver to get correct types
		GenericTypeResolver resolver = new GenericTypeResolver(processorClass);

		// init output before parameters, as pipeline attributes might
		// access this.
		createOutput(resolver);
		initKeys();
		initParameters(resolver);
	}

	/** Create all keys for this processor specification. */
	private void initKeys() throws ProcessorLayoutException {
		this.keys.addAll(KeySpecification.getKeys(this));
	}

	/**
	 * Create and initialize the parameters for this specification.
	 * 
	 * @param resolver
	 *            the resolver used for the clas currently initialized.
	 */
	private void initParameters(GenericTypeResolver resolver)
			throws DriverException, BlockFileException,
			ProcessorLayoutException {

		addParam(new ConditionalProcessorSpecificationParameter(this));

		for (ProcessorSpecificationParameter param : getParameters(
				processorClass, this, resolver)) {
			addParam(param);
		}
	}

	/** Returns all specified parameters for the given object. */
	/* package */static List<ProcessorSpecificationParameter> getParameters(
			Class<?> objectClass, ProcessorSpecification specification,
			GenericTypeResolver resolver) throws DriverException {

		List<ProcessorSpecificationParameter> parameters = new ArrayList<>();

		// handle all methods marked as conqat parameters
		parameters.addAll(MethodBasedProcessorSpecificationParameter
				.getParameters(objectClass, specification, resolver));

		// handle all fields marked as conqat field parameter
		parameters.addAll(FieldBasedProcessorSpecificationParameter
				.getParameters(objectClass, specification, resolver));

		// handle all fields marked as conqat parameter object
		parameters.addAll(ObjectBasedProcessorSpecificationParameter
				.getParameters(objectClass, specification, resolver));

		return parameters;
	}

	/** Create the (single) output. */
	private void createOutput(GenericTypeResolver resolver) {
		Method processMethod = null;
		for (Method m : processorClass.getMethods()) {
			if (m.getName().equals(PROCESSOR_WORKER_METHOD)
					&& m.getParameterTypes().length == 0) {

				// This is a work-around for the buggy sun compiler, as in
				// certain cases there are only bridge methods, so the
				// isBridge() flag can not be used to find the correct one.
				// Instead we use the one with the most specific return type.
				// See CR#3271 and CR#2361.
				if (processMethod == null
						|| (processMethod.getReturnType().isAssignableFrom(m
								.getReturnType()))) {
					processMethod = m;
				}
			}
		}

		if (processMethod == null) {
			throw new IllegalStateException(
					"Process method not found: this may not happen, as IConQATProcessor is implemented! Class: "
							+ processorClass.getName());
		}

		Class<?> outputType = resolver.resolveGenericType(processMethod
				.getGenericReturnType());
		defaultOutput = new ProcessorSpecificationOutput(this, outputType);
	}

	/**
	 * Determine the processor class and ensure that it implements interface
	 * {@link IConQATProcessor}, is annotated as {@link AConQATProcessor} and is
	 * no generic class, since we don't support generic processors.
	 * 
	 * @throws DriverException
	 *             if class was not found or on of the conditions above was
	 *             violated.
	 */
	private void determineClass() throws DriverException {
		try {
			processorClass = Class.forName(getName(), true, Thread
					.currentThread().getContextClassLoader());
		} catch (ClassNotFoundException e) {
			throw new EnvironmentException(
					EDriverExceptionType.PROCESSOR_CLASS_NOT_FOUND,
					"Processor class " + getName() + " not found.", this);
		}

		// make sure interface is implemented
		if (!IConQATProcessor.class.isAssignableFrom(processorClass)) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.PROCESSOR_CLASS_NOT_IMPLEMENTS_INTERFACE,
					"Processor class does not implement interface '"
							+ IConQATProcessor.class.getName() + "'.", this);
		}

		// check if class is annotated
		if (!processorClass.isAnnotationPresent(AConQATProcessor.class)) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.PROCESSOR_CLASS_NOT_ANNOTATED,
					"Class is not annotated as '"
							+ AConQATProcessor.class.getName() + "'.", this);
		}

		// we do not support generic processor classes
		if (processorClass.getTypeParameters().length > 0) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.GENERIC_PROCESSOR_CLASS,
					"Generic processor classes are not (yet?) supported!", this);
		}

		annotation = processorClass.getAnnotation(AConQATProcessor.class);
		threadSafe = processorClass
				.isAnnotationPresent(AThreadSafeProcessor.class);
	}

	/** Creates an instance of the underlying processor class. */
	public IConQATProcessor createProcessorInstance() throws DriverException {
		try {
			return (IConQATProcessor) processorClass.newInstance();
		} catch (InstantiationException e) {

			throw new ProcessorLayoutException(
					EDriverExceptionType.PROCESSOR_CLASS_WITHOUT_PARAMETERLESS_CONSTRUCTOR,
					"Processor class has no parameterless constructor.", this);

		} catch (IllegalAccessException e) {

			throw new ProcessorLayoutException(
					EDriverExceptionType.PROCESSOR_CLASS_WITHOUT_PUBLIC_CONSTRUCTOR,
					"Parameterless constructor of processor class is not public.",
					this);
		}
	}

	/** {@inheritDoc} */
	@Override
	public String getDoc() {
		return annotation.description();
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "Processor type '" + getName() + "'";
	}

	/** {@inheritDoc} */
	@Override
	public ProcessorSpecificationOutput[] getOutputs() {
		if (defaultOutput == null) {
			return new ProcessorSpecificationOutput[0];
		}
		return new ProcessorSpecificationOutput[] { defaultOutput };
	}

	/** Returns the key specifications of this processor. */
	public UnmodifiableList<KeySpecification> getKeys() {
		return CollectionUtils.asUnmodifiable(keys);
	}

	/** Returns the underlying processor class. */
	public Class<?> getProcessorClass() {
		return processorClass;
	}

	/** {@inheritDoc} */
	@Override
	protected ProcessorSpecificationParameter[] newParameterArray(int size) {
		return new ProcessorSpecificationParameter[size];
	}

	/** {@inheritDoc} */
	@Override
	public ErrorLocation getErrorLocation() {
		// return unknown location if processor class could not be determined
		if (processorClass == null) {
			return ErrorLocation.UNKNOWN;
		}

		return new ErrorLocation(processorClass);
	}

	/** Returns whether this processor is thread safe. */
	public boolean isThreadSafe() {
		return threadSafe;
	}
}