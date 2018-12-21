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
package org.conqat.engine.core.bundle.library.license;

/**
 * Base interface for license information containing the license name, license
 * website and content for a notice file.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47358 $
 * @ConQAT.Rating GREEN Hash: A6EFAFE801041414F38515ED5762619B
 */
public interface ILicense {

	/** The name of the license. */
	public String getName();

	/** The website with detailed information about the license. */
	public String getWebsite();

	/** Additional text displayed in a NOTICE file. */
	public String getNotice();

	/**
	 * Returns true if the license is compatible with
	 * {@link ELicense#APACHE_LICENSE_v2_0}.
	 */
	public boolean isApacheCompatible();

	/** Returns true if the license allows commercial use of the library. */
	public boolean isCommercialUseAllowed();
}
