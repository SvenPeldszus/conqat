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
package org.conqat.engine.dotnet.test.xunit;

import java.util.EnumSet;

import org.conqat.lib.commons.xml.ElementEnumSaxHandler.IElementEnum;

/**
 * XML element state graph for XUnit reports.
 * 
 * @author $Author: streitel $
 * @version $Rev: 50686 $
 * @ConQAT.Rating GREEN Hash: 88731F7FB1E8E7F3BFC6F0907C1BD156
 */
/* package */enum EXUnitElement implements IElementEnum<EXUnitElement> {
	/**
	 * The XML root element <code>assembly</code>.
	 */
	ASSEMBLY,

	/** The <code>test</code> elements which hold test results. */
	TEST;

	/** {@inheritDoc} */
	@Override
	public EnumSet<EXUnitElement> nextElements() {
		return EnumSet.of(TEST);
	}
}
