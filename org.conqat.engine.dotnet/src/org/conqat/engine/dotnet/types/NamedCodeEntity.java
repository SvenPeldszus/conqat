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
package org.conqat.engine.dotnet.types;

import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;

/**
 * Named code entity, such as a type, interface or class.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51681 $
 * @ConQAT.Rating YELLOW Hash: 163A9AEFA1561501BBE6473FDED5129A
 */
public class NamedCodeEntity extends CodeEntityBase {

	/** Name of the named code entity */
	private final String fqName;

	/** Constructor */
	public NamedCodeEntity(String fqName, ECodeEntityType type,
			ShallowEntity entity, String childSeparator) {
		super(type, entity, childSeparator);
		this.fqName = fqName;
	}

	/** Get fully qualified name */
	@Override
	public String getFqName() {
		return fqName;
	}

}
