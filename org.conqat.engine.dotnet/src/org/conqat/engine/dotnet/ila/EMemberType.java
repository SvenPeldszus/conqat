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
package org.conqat.engine.dotnet.ila;

/**
 * Possible types of ILA type members.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50996 $
 * @ConQAT.Rating RED Hash: 8D0562CCF3151A99B8EC870B614C634E
 */
public enum EMemberType {
	// TODO (FS) please use uppercase for these constants. make sure to
	// uppercase input strings before calling valueOf!
	/** A method. This includes property setters and getters. */
	Method,
	/** A constructor. */
	Constructor,
	/** A field. */
	Field,
	/** An event. */
	Event
}
