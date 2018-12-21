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

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;

import org.conqat.engine.commons.keys.IDependencyListKey;
import org.conqat.engine.commons.node.ListNode;
import org.conqat.engine.commons.node.NodeUtils;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * Smoke test for the {@link IlaXmlReader} class.
 * 
 * @author $Author: streitel $
 * @version $Revision: 49855 $
 * @ConQAT.Rating GREEN Hash: D1B2DF68B99CAC0676CFE681C295BA4F
 */
public class IlaXmlReaderTest extends ResourceProcessorTestCaseBase {

	/**
	 * Name of the test xml file containing resulting from a simple .net
	 * application.
	 */
	public static final String SIMPLE_XML = "Simple.xml";

	/**
	 * Name of the test xml file containing resulting from a .net application
	 * with yield statements.
	 */
	public static final String YIELD_XML = "Yield.exe.xml";

	/** XML reader smoke test. */
	public void testParseFile() throws Exception {
		parseTestFile(SIMPLE_XML);
	}

	/** Parses a test file and returns the XML reader. */
	private IlaXmlReader parseTestFile(String xmlFile) throws IOException,
			ConQATException {
		String content = FileSystemUtils.readFile(useTestFile(xmlFile));
		IlaXmlReader reader = new IlaXmlReader(dummyTextElement(content),
				new ListNode(), new HashSet<String>(), new HashSet<String>());
		reader.parse();
		return reader;
	}

	/** Tests removal of synthetic code resulting from yield statements. */
	public void testRemovingSyntheticYieldCode() throws IOException,
			ConQATException {
		IlaXmlReader xmlReader = parseTestFile(YIELD_XML);

		ListNode[] children = xmlReader.root.getChildren();
		assertEquals(1, children.length);

		ListNode type = children[0];
		Collection<String> dependencies = NodeUtils.getStringCollection(type,
				IDependencyListKey.DEPENDENCY_LIST_KEY);

		assertEquals(1, dependencies.size());
		assertEquals("Yield.Provider", dependencies.iterator().next());
	}
}