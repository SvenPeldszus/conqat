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
package org.conqat.engine.resource.analysis;

import java.security.MessageDigest;
import java.util.Arrays;
import java.util.List;

import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.resource.util.UniformPathUtils;
import org.conqat.lib.commons.collections.ListMap;
import org.conqat.lib.commons.digest.Digester;
import org.conqat.lib.commons.digest.MD5Digest;

/**
 * Support class for processors that deal with duplicate elements. Duplicate
 * elements are detected using a simple hash-based approach.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49466 $
 * @ConQAT.Rating GREEN Hash: 36D3E928B7FE5B2A6A3888D617D1033F
 */
public abstract class DuplicateElementProcessorSupport implements
		IConQATParameterObject {

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "equal-name-required", attribute = "value", description = ""
			+ "Duplicate elements are required to have an equal name, not only equal content.", optional = true)
	public boolean isEqualNameRequired = false;

	/**
	 * Maps from the hashed element to a list of element. So every list with
	 * length > 2 describes a set of duplicate elements.
	 */
	private final ListMap<HashedElement, IElement> hashedElements = new ListMap<HashedElement, IElement>();

	/**
	 * Performs the detection of duplicate elements and calls
	 * {@link #processDuplicate(List)} for each set of detected duplicate
	 * elements.
	 */
	public void processDuplicates(IResource input) throws ConQATException {
		for (IElement element : ResourceTraversalUtils.listElements(input,
				IElement.class)) {
			hashedElements.add(new HashedElement(element), element);
		}

		for (HashedElement hashedElement : hashedElements.getKeys()) {
			List<IElement> elementList = hashedElements
					.getCollection(hashedElement);
			if (elementList.size() > 1) {
				processDuplicate(elementList);
			}
		}
	}

	/**
	 * Template method to process a set of duplicate elements. Due to
	 * implementations issues the provided type is a list. However, semantically
	 * it is a set as every element is guaranteed to occur only once and the
	 * ordering is undefined. Length of the list is guaranteed to be >=2.
	 */
	protected abstract void processDuplicate(List<IElement> elements);

	/**
	 * A class that allows hashing of elements. The hash code of this class is
	 * defined by a MD5 digest of the element, the <code>equals()</code> method
	 * compares element contents.
	 */
	private class HashedElement {

		/** The element associated with the hashed element. */
		private final IElement element;

		/** The digest of the element. */
		private final MD5Digest digest;

		/** The content of the element. */
		private byte[] content;

		/** Create new hashed element. */
		public HashedElement(IElement element) throws ConQATException {
			this.element = element;
			content = element.getContent();

			MessageDigest digester = Digester.getMD5();
			digester.reset();
			digester.update(content);
			if (isEqualNameRequired) {
				byte[] elementName = getElementName().getBytes();
				digester.update(elementName);
			}
			digest = new MD5Digest(digester);
		}

		/** Returns the name of the element. */
		private String getElementName() {
			return UniformPathUtils.getElementName(element.getUniformPath());
		}

		/** Hash code is based on the digest. */
		@Override
		public int hashCode() {
			return digest.hashCode();
		}

		/** Compares element contents. */
		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof HashedElement)) {
				return false;
			}

			HashedElement otherElement = (HashedElement) obj;

			if (!otherElement.digest.equals(digest)) {
				return false;
			}

			if (isEqualNameRequired
					&& !getElementName().equals(otherElement.getElementName())) {
				return false;
			}

			return Arrays.equals(content, otherElement.content);
		}
	}
}