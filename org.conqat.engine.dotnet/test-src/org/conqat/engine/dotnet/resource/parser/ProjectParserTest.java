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
package org.conqat.engine.dotnet.resource.parser;

import java.util.Set;
import java.util.TreeSet;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.resource.BuildConfiguration;
import org.conqat.engine.dotnet.resource.parser.ProjectParser.VSProject;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Test cases for {@link ProjectParser}.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51330 $
 * @ConQAT.Rating GREEN Hash: F1C70138AAD6125348C5EAB1959B24AF
 */
public class ProjectParserTest extends ResourceProcessorTestCaseBase {

	/** Elements of the 2003 console exe project. */
	private static final String[] NUNIT_CONSOLE_EXE_ELEMENTS_2003 = new String[] {
			"..\\..\\CommonAssemblyInfo.cs", "Class1.cs", "assemblyinfo.cs" };

	/** Elements of the console exe project. */
	private static final String[] NUNIT_CONSOLE_EXE_ELEMENTS = new String[] {
			"..\\..\\CommonAssemblyInfo.cs", "Class1.cs" };

	/** Elements of the core tests project. */
	private static final String[] NUNIT_CORE_TEST_ELEMENTS = new String[] {
			"..\\..\\CommonAssemblyInfo.cs", "AllTests.cs",
			"AssemblyReaderTests.cs", "AssemblyResolverTests.cs",
			"AssemblyTests.cs", "AssemblyVersionFixture.cs",
			"AttributeDescriptionFixture.cs", "BasicRunnerTests.cs",
			"CallContextTests.cs", "CategoryAttributeTests.cs",
			"CoreExtensionsTests.cs", "DirectorySwapperTests.cs",
			"EventQueueTests.cs", "EventTestFixture.cs",
			"ExpectExceptionTest.cs", "FailFixture.cs",
			"FixtureSetupTearDownTest.cs", "IgnoreFixture.cs",
			"LegacySuiteTests.cs", "MockTestRunner.cs", "NameFilterTest.cs",
			"NamespaceAssemblyTests.cs", "PlatformDetectionTests.cs",
			"PropertyAttributeTests.cs", "ReflectTests.cs",
			"RemoteRunnerTests.cs", "SerializationBug.cs",
			"SetUpFixtureTests.cs", "SetUpTest.cs", "SimpleNameFilterTests.cs",
			"SimpleTestRunnerTests.cs", "StackOverflowTestFixture.cs",
			"SuiteBuilderTests.cs", "SuiteBuilderTests_Multiple.cs",
			"TestAssemblyBuilderTests.cs", "TestCaseResultFixture.cs",
			"TestCaseTest.cs", "TestConsole.cs", "TestDelegateFixture.cs",
			"TestFixtureBuilderTests.cs", "TestFixtureExtension.cs",
			"TestFixtureTests.cs", "TestFrameworkTests.cs", "TestIdTests.cs",
			"TestInfoTests.cs", "TestNameTests.cs", "TestNodeTests.cs",
			"TestRunnerThreadTests.cs", "TestSuiteResultFixture.cs",
			"TestSuiteTest.cs", "ThreadedTestRunnerTests.cs",
			"UnhandledExceptionTests.cs", "XmlTest.cs", };

	/** Elements of the console project. */
	private static final String[] NUNIT_CONSOLE_ELEMENTS = new String[] {
			"..\\..\\CommonAssemblyInfo.cs", "AssemblyInfo.cs",
			"ConsoleOptions.cs", "ConsoleUi.cs" };

	/** Tests for the Nunit Console Runner project. */
	public void testNunitConsole() throws Exception {
		// VS 2003
		VSProject project = assertProjectSources("nunit-console_VS2003.csproj",
				NUNIT_CONSOLE_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug", null),
				"bin\\Debug\\nunit-console-runner.dll", 4,
				new Integer[] { 618 });
		assertProjectProperties(project,
				new BuildConfiguration("Release", null),
				"bin\\Release\\nunit-console-runner.dll", 4,
				new Integer[] { 618 });
		assertProjectProperties(project, new BuildConfiguration("Foo", null),
				null, 4, new Integer[] {});

		// VS 2005
		project = assertProjectSources("nunit-console_VS2005.csproj",
				NUNIT_CONSOLE_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit-console-runner.dll", 4,
				new Integer[] { 618, 1699, 1701, 1702 });
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit-console-runner.dll", 4,
				new Integer[] { 618, 1699, 1701, 1702 });
		assertProjectProperties(project, new BuildConfiguration("Foo", "Bar"),
				null, 4, new Integer[] {});

		// VS 2008
		project = assertProjectSources("nunit-console_VS2008.csproj",
				NUNIT_CONSOLE_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit-console-runner.dll", 4,
				new Integer[] { 618, 1699, 1701, 1702 });
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit-console-runner.dll", 4,
				new Integer[] { 618, 1699, 1701, 1702 });
	}

	/** Tests for the Nunit Console project. */
	public void testNunitConsoleExe() throws Exception {
		// VS 2003
		VSProject project = assertProjectSources(
				"nunit-console.exe_VS2003.csproj",
				NUNIT_CONSOLE_EXE_ELEMENTS_2003);
		assertProjectProperties(project, new BuildConfiguration("Debug", null),
				"bin\\Debug\\nunit-console.exe", 4, new Integer[] {});
		assertProjectProperties(project,
				new BuildConfiguration("Release", null),
				"bin\\Release\\nunit-console.exe", 4, new Integer[] {});

		// VS 2005
		project = assertProjectSources("nunit-console.exe_VS2005.csproj",
				NUNIT_CONSOLE_EXE_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit-console.exe", 4,
				new Integer[] {});
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit-console.exe", 4,
				new Integer[] {});

		// VS 2008
		project = assertProjectSources("nunit-console.exe_VS2008.csproj",
				NUNIT_CONSOLE_EXE_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit-console.exe", 4,
				new Integer[] {});
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit-console.exe", 4,
				new Integer[] {});
	}

	/** Tests for the Nunit Core Test project. */
	public void testNunitCoreTest() throws Exception {
		// VS 2003
		VSProject project = assertProjectSources(
				"nunit.core.tests_VS2003.csproj", NUNIT_CORE_TEST_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug", null),
				"bin\\Debug\\nunit.core.tests.dll", 4, new Integer[] { 618 });

		assertProjectProperties(project,
				new BuildConfiguration("Release", null),
				"bin\\Release\\nunit.core.tests.dll", 2, new Integer[] {});

		// VS 2005
		project = assertProjectSources("nunit.core.tests_VS2005.csproj",
				NUNIT_CORE_TEST_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit.core.tests.dll", 4,
				new Integer[] { 618, 1699 });
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit.core.tests.dll", 4,
				new Integer[] { 618, 666, 1699 });

		// VS 2008
		project = assertProjectSources("nunit.core.tests_VS2008.csproj",
				NUNIT_CORE_TEST_ELEMENTS);
		assertProjectProperties(project, new BuildConfiguration("Debug2005",
				"AnyCPU"), "bin\\Debug2005\\nunit.core.tests.debug.dll", 4,
				new Integer[] { 618, 1699 });
		assertProjectProperties(project, new BuildConfiguration("Release2005",
				"AnyCPU"), "bin\\Release2005\\nunit.core.tests.dll", 4,
				new Integer[] { 618, 1699 });
	}

	/**
	 * Parses the project, asserts it to contain the given sources and returns
	 * the {@link VSProject} object.
	 */
	private VSProject assertProjectSources(String projectPath, String[] elements)
			throws Exception {
		ITextElement projectElement = getProjectElement(projectPath);
		VSProject project = ProjectParser.parse(projectElement);
		Set<String> relativeSources = new TreeSet<>(
				project.getRelativeSources());
		assertSameElements("different source elements", elements,
				relativeSources);

		return project;
	}

	/** Asserts that the given project properties match. */
	private void assertProjectProperties(VSProject project,
			BuildConfiguration config, String relativeAssemblyName,
			int warnLevel, Integer[] noWarnIds) {
		String assemblyName;
		try {
			assemblyName = project.getRelativeAssemblyName(config);
		} catch (ConQATException e) {
			assemblyName = null;
		}
		assertEquals("relative assembly name not matching",
				relativeAssemblyName, assemblyName);

		assertEquals("different warn level", warnLevel,
				project.getWarningLevel(config));
		assertSameElements("different no warn ids", noWarnIds,
				new TreeSet<Integer>(project.getNoWarnIds(config)));
	}

	/** @return the project parser for a visual studio project. */
	private ITextElement getProjectElement(String filename) throws Exception {
		ITextResource scope = createTextScope(getTestDataDirectory(),
				new String[] { filename }, new String[] {});
		return ResourceTraversalUtils.getSingleTextElement(scope);
	}

	/**
	 * Asserts that a list contains the given elements by concatenating all
	 * elements as a line separated string.
	 */
	private static <T> void assertSameElements(String message, T[] expected,
			Iterable<T> elements) {
		assertEquals(message, StringUtils.concat(expected, StringUtils.CR),
				StringUtils.concat(elements, StringUtils.CR));
	}
}