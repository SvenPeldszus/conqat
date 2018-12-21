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

import java.util.List;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.ProcessorInfoMock;
import org.conqat.engine.dotnet.types.CodeEntityBase.ECodeEntityType;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.TokenTestCaseBase;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.IToken;

/**
 * Test cases for {@link CodeEntityFactory}.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51681 $
 * @ConQAT.Rating YELLOW Hash: 30B237D8ACB643E51A2C6EC13DF8B4A8
 */
public class CodeEntityFactoryTest extends TokenTestCaseBase {

	/**
	 * Make sure that shallow parser survives class keywords in parent
	 * declaration
	 */
	public void testClassKeywordBug() throws ConQATException {
		CodeEntityBase root = parseTestFile("classkeyword.cs");
		assertEquals(1, root.getChildren().size());

		CodeEntityBase clazz = root.getChildren().get(0).getChildren().get(0);
		assertTypeAndFqName(clazz, ECodeEntityType.TYPE, "A.Controller<T0>");
	}

	/**
	 * Make sure that shallow parser works for interface with generic methods
	 */
	public void testClassKeywordBug2() throws ConQATException {
		CodeEntityBase root = parseTestFile("interface.cs");
		assertEquals(1, root.getChildren().size());

		CodeEntityBase clazz = root.getChildren().get(0);
		assertTypeAndFqName(clazz, ECodeEntityType.TYPE, "IInterface");
	}

	/** Test parsing of hand-constructed file with simple examples */
	public void testParseSimpleCodeEntities() throws ConQATException {
		CodeEntityBase root = parseTestFile("codeEntities.cs");

		assertEquals(2, root.getChildren().size());

		CodeEntityBase namespace1 = root.getChildren().get(0);
		assertTypeAndFqName(namespace1, ECodeEntityType.NAMESPACE,
				"NUnit.UiKit");

		assertEquals(2, namespace1.getChildren().size());
		CodeEntityBase clazz1 = namespace1.getChildren().get(0);
		assertTypeAndFqName(clazz1, ECodeEntityType.TYPE, "NUnit.UiKit.MyClass");
		CodeEntityBase interfaz1 = namespace1.getChildren().get(1);
		assertTypeAndFqName(interfaz1, ECodeEntityType.TYPE,
				"NUnit.UiKit.IInterface");

		assertTypeAndFqName(root.getChildren().get(1),
				ECodeEntityType.NAMESPACE, "NUnit.UiKit2");
	}

	/** Asserts that generic class names are parsed correctly */
	public void testParseGenericClassNames() throws Exception {
		CodeEntityBase root = parseTestFile("generictypes.cs");

		CodeEntityBase namespace = root.getChildren().get(0);
		assertTypeAndFqName(namespace, ECodeEntityType.NAMESPACE,
				"GenericsTestLibrary");
		assertEquals(7, namespace.getChildren().size());

		assertTypeAndFqName(namespace.getChildren().get(0),
				ECodeEntityType.TYPE, "GenericsTestLibrary.Test<T0>");
		assertTypeAndFqName(namespace.getChildren().get(1),
				ECodeEntityType.TYPE, "GenericsTestLibrary.SampleDelegate");
		assertTypeAndFqName(namespace.getChildren().get(2),
				ECodeEntityType.TYPE, "GenericsTestLibrary.Class1");
		assertTypeAndFqName(namespace.getChildren().get(3),
				ECodeEntityType.TYPE, "GenericsTestLibrary.Class1<T0,T1>");
		assertTypeAndFqName(namespace.getChildren().get(4),
				ECodeEntityType.TYPE, "GenericsTestLibrary.Class1<T0,T1>");
		assertTypeAndFqName(namespace.getChildren().get(5),
				ECodeEntityType.TYPE, "GenericsTestLibrary.Class1<T0>");
		assertTypeAndFqName(namespace.getChildren().get(6),
				ECodeEntityType.TYPE,
				"GenericsTestLibrary.IInterface<T0,T1,T2>");
	}

	/** Asserts that parsing an empty file will not raise an error. */
	public void testParsingEmptyFile() throws Exception {
		CodeEntityBase root = parseTestFile("empty.cs");

		assertSame(root.getChildren().size(), 0);
		assertSame(root.collectTypeNames().size(), 0);
		assertSame(root.getFqName(), null);
	}

	/**
	 * Make sure that method entity collection works.
	 */
	public void testMethods() throws ConQATException {
		CodeEntityBase root = parseTestFile("interface.cs");
		List<NamedCodeEntity> methods = root.collectMethods();
		assertEquals(1, methods.size());
		assertEquals("IInterface/GetService", methods.get(0).getFqName());
	}

	/**
	 * Make sure that method entity collection works.
	 */
	public void testMethods2() throws ConQATException {
		CodeEntityBase root = parseTestFile("methods.cs");
		List<NamedCodeEntity> methods = root.collectMethods();
		assertEquals(2, methods.size());
		assertEquals("A.Controller<T0>/Foo", methods.get(0).getFqName());
		assertEquals("A.Controller<T0>/ContextObject", methods.get(1)
				.getFqName());
	}

	/**
	 * Run shallow parser and code entity extraction on a file from test-data
	 * folder
	 */
	private CodeEntityBase parseTestFile(String filename)
			throws ConQATException {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), ELanguage.CS);

		List<IToken> tokens = element.getTokens(new ProcessorInfoMock()
				.getLogger());

		return CodeEntityFactory.codeEntitiesFor(tokens);
	}

	/** Asserts name and type of a {@link NamedCodeEntity} */
	private void assertTypeAndFqName(CodeEntityBase entity,
			ECodeEntityType expectedType, String expectedName) {
		assertEquals(expectedType, ((NamedCodeEntity) entity).getType());
		assertEquals(expectedName, entity.getFqName());
	}

}
