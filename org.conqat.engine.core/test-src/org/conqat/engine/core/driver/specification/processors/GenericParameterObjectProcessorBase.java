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

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATParameterObject;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.core.core.IConQATProcessor;

/**
 * Processor for testing purposes.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48563 $
 * @ConQAT.Rating GREEN Hash: 5FDD10D9B5012977D165B56195FF23F0
 */
public abstract class GenericParameterObjectProcessorBase<E extends Number, F>
		implements IConQATProcessor {

	/** test parameter object with bound parameter. */
	@AConQATParameterObject(namePrefix = "bound")
	public BoundParameter boundParam = new BoundParameter();

	/** test parameter object with bound parameter. */
	@AConQATParameterObject(namePrefix = "specified")
	public Parameter<Integer> specifiedParam = new Parameter<Integer>();

	/** test parameter object with inherited parameter type. */
	@AConQATParameterObject(namePrefix = "inherited")
	public Parameter<E> inheritedParam = new Parameter<E>();

	/** test parameter object with doubly inherited parameter type. */
	@AConQATParameterObject(namePrefix = "doubly-inherited")
	public InheritedParameter<Long, F> doublyInheritedParam = new InheritedParameter<Long, F>();

	/** test nested parameter object. */
	@AConQATParameterObject(namePrefix = "nested")
	public NestedParameter<Short> nestedParam = new NestedParameter<Short>();

	/** test parameter object with type from enclosing type. */
	@AConQATParameterObject(namePrefix = "enclosing")
	public EnclosingParamter enclosingParam = new EnclosingParamter();

	/** Generic parameter object in super class. */
	public static class BoundParameter extends Parameter<Byte> {
		// just proxy
	}

	/** Generic parameter object. */
	public static class Parameter<T> implements IConQATParameterObject {

		/** test field */
		@AConQATFieldParameter(parameter = "field", attribute = "attr", description = "")
		public T exposed;
	}

	/** Doubly inherited generic parameter object. */
	public static class InheritedParameter<S, T> extends Parameter<T> {

		/** test field */
		@AConQATFieldParameter(parameter = "field2", attribute = "attr", description = "")
		public S exposed2;
	}

	/** Nested generic parameter object. */
	public static class NestedParameter<U> implements IConQATParameterObject {

		/** test parameter object. */
		@AConQATParameterObject()
		public Parameter<U> param = new Parameter<U>();
	}

	/** Generic parameter object with type argument from enclosing type. */
	public class EnclosingParamter implements IConQATParameterObject {

		/** test field */
		@AConQATFieldParameter(parameter = "field", attribute = "attr", description = "")
		public E exposed;
	}
}