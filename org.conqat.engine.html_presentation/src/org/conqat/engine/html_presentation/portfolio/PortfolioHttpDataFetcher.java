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
package org.conqat.engine.html_presentation.portfolio;

import java.io.IOException;
import java.net.MalformedURLException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.GetMethod;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * Fetches the dashboard statistics JSON data from an HTTP source.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51577 $
 * @ConQAT.Rating GREEN Hash: 5C710DA45552C7537914FD261CF066B1
 */
public class PortfolioHttpDataFetcher extends PortfolioDataFetcherBase {

	/** The HttpClient with which to make the request. */
	protected HttpClient client;

	/** Constructor. */
	public PortfolioHttpDataFetcher(String name, String locationUrl) {
		super(name, locationUrl);
		client = new HttpClient();
	}

	/** {@inheritDoc} */
	@Override
	protected String getJsonString(String dataJsonFileLocation)
			throws ConQATException, IOException {
		try {
			GetMethod method = new GetMethod(dataJsonFileLocation);
			client.executeMethod(method);
			return FileSystemUtils.readStream(method.getResponseBodyAsStream());
		} catch (MalformedURLException e) {
			throw new ConQATException("Unable to parse URL "
					+ dataJsonFileLocation, e);
		}
	}
}
