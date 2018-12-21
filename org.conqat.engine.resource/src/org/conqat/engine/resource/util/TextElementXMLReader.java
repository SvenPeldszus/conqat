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
package org.conqat.engine.resource.util;

import org.conqat.engine.commons.util.ConQATXMLReader;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.lib.commons.xml.IXMLResolver;
import org.conqat.lib.commons.xml.XMLResolver;

/**
 * Base class for XML readers that read from {@link ITextElement}s.
 * 
 * @author $Author: poehlmann $
 * @version $Rev: 49442 $
 * @ConQAT.Rating GREEN Hash: A8A96D564750740A4DD8AA86AF9512DE
 */
public class TextElementXMLReader<E extends Enum<E>, A extends Enum<A>, X extends Exception>
		extends ConQATXMLReader<E, A, X> {

	/** The element this reader is working on. */
	protected final ITextElement element;

	/** Constructor. */
	public TextElementXMLReader(ITextElement element, Class<A> attributeClass)
			throws ConQATException {
		this(element, new XMLResolver<E, A>(attributeClass));
	}

	/** Constructor with XMLResolver. */
	public TextElementXMLReader(ITextElement element,
			IXMLResolver<E, A> resolver) throws ConQATException {
		super(element.getTextContent(), resolver);
		this.element = element;
	}

	/** Returns location of the element. */
	@Override
	protected String getLocation() {
		return element.getLocation();
	}
}