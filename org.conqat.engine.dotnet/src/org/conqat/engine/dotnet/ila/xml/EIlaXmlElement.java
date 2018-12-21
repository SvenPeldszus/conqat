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
package org.conqat.engine.dotnet.ila.xml;

/**
 * Enumeration of the XML elements of the IL-XML format.
 * 
 * @author Elmar Juergens
 * @author $Author: streitel $
 * 
 * @version $Revision: 50996 $
 * @ConQAT.Rating RED Hash: EDEF73B9E7BBDB787F833BEB059CAD91
 */
public enum EIlaXmlElement {

	/** TypeElement represents types such as classes, enums, ... */
	TypeElement,

	/** Expresses implementation inheritance relationship between types */
	// TODO (FS) this comment is a bit crpytic. how about: contains the
	// extended classes of a type
	Extends,

	/** Represents interface inheritance relationship between types */
	// TODO (FS) this comment is a bit crpytic. how about: contains the
	// implemented inerfaces of a type
	Implements,

	/**
	 * Represents dependencies that do not originate in inheritance
	 * relationships
	 */
	Depends,

	/** Represents a member of a type */
	Member,

	/** Represents a method parameter. */
	Parameter
}