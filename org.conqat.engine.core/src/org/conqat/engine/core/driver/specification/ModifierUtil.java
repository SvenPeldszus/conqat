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
package org.conqat.engine.core.driver.specification;

import java.lang.reflect.Member;
import java.lang.reflect.Modifier;

import org.conqat.engine.core.driver.error.EDriverExceptionType;
import org.conqat.engine.core.driver.error.IErrorLocatable;
import org.conqat.engine.core.driver.error.ProcessorLayoutException;

/**
 * Utility class to perform checks on class {@link Member} modifiers.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49465 $
 * @ConQAT.Rating GREEN Hash: 062F230E56D9C8A9E1D734D36BBF15B4
 */
/* package */class ModifierUtil {

	/** Throws a {@link ProcessorLayoutException} if the member is static. */
	public static void assertNotStatic(Member member, IErrorLocatable locatable)
			throws ProcessorLayoutException {
		if (Modifier.isStatic(member.getModifiers())) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.STATIC_PARAMETER,
					member.getClass().getName() + " " + member.getName()
							+ " is static!", locatable);
		}
	}

	/** Throws a {@link ProcessorLayoutException} if the member not public. */
	public static void assertPublic(Member member, IErrorLocatable locatable)
			throws ProcessorLayoutException {
		if (!Modifier.isPublic(member.getModifiers())) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.NOT_PUBLIC_PARAMETER, member
							.getClass().getName()
							+ " "
							+ member.getName()
							+ " is not public!", locatable);
		}
	}

	/** Throws a {@link ProcessorLayoutException} if the member is final. */
	public static void assertNotFinal(Member member, IErrorLocatable locatable)
			throws ProcessorLayoutException {
		if (Modifier.isFinal(member.getModifiers())) {
			throw new ProcessorLayoutException(
					EDriverExceptionType.FINAL_FIELD_PARAMETER, member
							.getClass().getName()
							+ " "
							+ member.getName()
							+ " is final!", locatable);
		}
	}
}
