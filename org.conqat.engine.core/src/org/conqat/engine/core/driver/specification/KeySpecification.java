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
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.error.ProcessorLayoutException;
import org.conqat.engine.core.driver.util.IDocumented;
import org.conqat.lib.commons.reflect.ReflectionUtils;

/**
 * Specification of keys read or written by a processor. This is used to make
 * the {@link AConQATKey} information available.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49407 $
 * @ConQAT.Rating GREEN Hash: 5B94459DA72B3837A886B2C3D7F71985
 */
public class KeySpecification implements IDocumented {

	/** Name of the key. */
	private final String name;

	/** Documentation for this key. */
	private final String doc;

	/** Type of this key. */
	private final String type;

	/** Create new KeySpecification. */
	public KeySpecification(String name, String type, String description) {
		this.name = name;
		this.type = type;
		doc = description;
	}

	/** Returns the documentation of this key. */
	@Override
	public String getDoc() {
		return doc;
	}

	/** Returns the name of this key. */
	@Override
	public String getName() {
		return name;
	}

	/** Returns the type of this key. */
	public String getType() {
		return type;
	}

	/**
	 * Returns all {@link AConQATKey} annotated fields of a processor class as
	 * {@link KeySpecification}s.
	 */
	public static List<KeySpecification> getKeys(
			ProcessorSpecification specification)
			throws ProcessorLayoutException {
		List<KeySpecification> keys = new ArrayList<>();
		for (Field field : ReflectionUtils.getAllFields(specification
				.getProcessorClass())) {
			if (field.isAnnotationPresent(AConQATKey.class)) {
				int modifiers = field.getModifiers();
				if (!Modifier.isStatic(modifiers)
						|| !Modifier.isFinal(modifiers)
						|| !Modifier.isPublic(modifiers)) {
					throw newProcessorLayoutException(
							EDriverExceptionType.KEY_NOT_PUBLIC_STATIC_FINAL,
							specification, field);
				}
				if (!field.getType().equals(String.class)) {
					throw newProcessorLayoutException(
							EDriverExceptionType.KEY_NOT_STRING, specification,
							field);
				}
				AConQATKey keyAnnotation = field
						.getAnnotation(AConQATKey.class);
				try {
					keys.add(new KeySpecification(((String) field.get(null)),
							keyAnnotation.type(), keyAnnotation.description()));
				} catch (IllegalAccessException e) {
					throw new IllegalStateException(
							"This should not happen as we checked for public!",
							e);
				}
			}
		}
		return keys;
	}

	/**
	 * Creates a new instance of a {@link ProcessorLayoutException} containing
	 * the specification name and field name in its name.
	 */
	private static ProcessorLayoutException newProcessorLayoutException(
			EDriverExceptionType type, ProcessorSpecification specification,
			Field field) {
		return new ProcessorLayoutException(type, specification.toString()
				+ ": field " + field.getName(), specification);
	}
}