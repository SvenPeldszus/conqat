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
package org.conqat.engine.sourcecode.analysis.shallowparsed;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.sourcecode.shallowparser.IShallowParser;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.test.CCSMTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Tests the {@link ShallowParsingUtils}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 48801 $
 * @ConQAT.Rating GREEN Hash: E9390F9CF31AD8F2E7E005DCF161AE83
 */
public class ShallowParsingUtilsTest extends CCSMTestCaseBase {

	/**
	 * Tests the
	 * {@link ShallowParsingUtils#extractParameterNameTokens(org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity)}
	 * method.
	 */
	public void testExtractParameterNameTokens() throws ConQATException {
		String code = "int foo(int a, const char &b, double c = 17, "
				+ "MyFancyType d[], MyFancyType2[] e, Foo* f, "
				+ "std::vector<int> &g, map<string, vector<int> > h) {}";
		List<IToken> tokens = ScannerUtils.getTokens(code, ELanguage.CPP);
		List<ShallowEntity> entities = ShallowParserFactory.createParser(
				ELanguage.CPP).parseTopLevel(tokens);

		assertEquals(1, entities.size());
		assertEquals(EShallowEntityType.METHOD, entities.get(0).getType());

		List<IToken> parameterTokens = ShallowParsingUtils
				.extractParameterNameTokens(entities.get(0));
		StringBuilder names = new StringBuilder();
		for (IToken token : parameterTokens) {
			names.append(token.getText() + " ");
		}

		assertEquals("a b c d e f g h ", names.toString());
	}

	/** Test if the constant detection works for constants in interfaces */
	public void testIsCostant() throws ConQATException, IOException {
		File javaInterface = useTestFile("interfaceConstant.java");
		List<IToken> tokens = ScannerUtils.getTokens(
				FileSystemUtils.readFile(javaInterface), ELanguage.JAVA);
		IShallowParser parser = ShallowParserFactory
				.createParser(ELanguage.JAVA);
		List<ShallowEntity> entities = parser.parseTopLevel(tokens);

		assertEquals(1, entities.size());

		assertTrue(
				"Constant not recognized.",
				ShallowParsingUtils.isConstant(entities.get(0).getChildren()
						.get(0)));
	}
}
