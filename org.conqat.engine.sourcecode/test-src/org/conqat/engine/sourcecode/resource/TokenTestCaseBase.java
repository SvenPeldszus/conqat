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
package org.conqat.engine.sourcecode.resource;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.conqat.engine.commons.findings.FindingsList;
import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATProcessor;
import org.conqat.engine.core.logging.ELogLevel;
import org.conqat.engine.core.logging.testutils.CollectingLogger;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.IElement;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.binary.BinaryElementFactory;
import org.conqat.engine.resource.build.IElementFactory;
import org.conqat.engine.resource.build.ResourceBuilder;
import org.conqat.engine.resource.scope.filesystem.FileContentAccessor;
import org.conqat.engine.resource.scope.filesystem.FileSystemScope;
import org.conqat.engine.resource.scope.filesystem.SingleFileScope;
import org.conqat.engine.resource.scope.filter.ContentAccessorPathFilter;
import org.conqat.engine.resource.scope.memory.InMemoryContentAccessor;
import org.conqat.engine.resource.scope.zip.ZipFileScope;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.engine.resource.util.AntPatternListDef;
import org.conqat.engine.sourcecode.shallowparser.ShallowParserFactory;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntity;
import org.conqat.engine.sourcecode.shallowparser.framework.ShallowEntityTraversalUtils.ShallowEntityVisitorBase;
import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.PairList;
import org.conqat.lib.commons.filesystem.CanonicalFile;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.cqddl.function.CQDDLEvaluationException;
import org.conqat.lib.cqddl.function.ICQDDLFunction;
import org.conqat.lib.scanner.ELanguage;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.IToken;
import org.conqat.lib.scanner.ScannerFactory;
import org.conqat.lib.scanner.ScannerUtils;

/**
 * Base classes for token-related tests. Contains utility methods for working
 * with {@link ITokenElement}s, scanners, shallow parsers and findings.
 * 
 * @author $Author: streitel $
 * @version $Rev: 51565 $
 * @ConQAT.Rating YELLOW Hash: C40CC4311E6E415DF2FDEC4E34CFB149
 */
public abstract class TokenTestCaseBase extends ResourceProcessorTestCaseBase {

	/** Constructor. */
	protected TokenTestCaseBase() {
		ICQDDLFunction tokenFactory = new ICQDDLFunction() {
			@Override
			public Object eval(PairList<String, Object> arg0)
					throws CQDDLEvaluationException {
				try {
					return executeProcessor(TokenElementFactory.class, "()");
				} catch (Exception e) {
					throw new CQDDLEvaluationException(
							"Failed to construct factory!", e);
				}
			}
		};
		parsingParameters.registerFunction("tokenFactory", tokenFactory);
	}

	/**
	 * Create a new token element with the specified content. The language is
	 * {@link ELanguage#JAVA}. The uniform path is "foo".
	 */
	protected ITokenElement createTokenElement(String content) throws Exception {
		return createTokenElement(content, ELanguage.JAVA);
	}

	/**
	 * Create a new token element with the specified content and language. The
	 * uniform path is "foo".
	 */
	protected ITokenElement createTokenElement(String content,
			ELanguage language) throws Exception {
		IElementFactory factory = (IElementFactory) executeProcessor(
				TokenElementFactory.class, "(language=(name=", language, "))");
		// we use the bytes representation below, as we need a non-string object
		// as all strings are interpreted as part of the CQDDL expression. This
		// is problematic if the string contains quotes.
		IContentAccessor[] accessors = (IContentAccessor[]) parseCQDDL(
				"memScope(foo=", content.getBytes(), ")");
		IElement element = factory.create(accessors[0]);

		assertTrue(element instanceof ITokenElement);
		return (ITokenElement) element;
	}

	/**
	 * Creates a new token element with the specified content.
	 * 
	 * @param testFile
	 *            File that contains content of token element
	 * @param language
	 *            Language of file
	 */
	protected ITokenElement createTokenElement(CanonicalFile testFile,
			ELanguage language) {
		IContentAccessor accessor = new FileContentAccessor(testFile,
				testFile.getParentFile(), "TEST");
		return new TokenElement(accessor, Charset.defaultCharset(), language);
	}

	/**
	 * Creates a new token element with the specified content.
	 */
	protected ITokenElement createTokenElement(CanonicalFile testFile,
			String uniformPath, ELanguage language) {
		IContentAccessor accessor = new FileContentAccessor(testFile,
				uniformPath);
		return new TokenElement(accessor, Charset.defaultCharset(), language);
	}

	/**
	 * Retrieves tokens from an element and checks if the returned tokens have
	 * the expected types. It is also checked if the correct number of log
	 * messages is generated at the specified log level.
	 * */
	protected void assertTokens(ITokenElement element, ELogLevel minLogLevel,
			int expectedMessages, ETokenType... types) throws ConQATException {
		CollectingLogger logger = new CollectingLogger(minLogLevel);
		Iterator<IToken> it = element.getTokens(logger).iterator();

		for (ETokenType type : types) {
			if (!it.hasNext()) {
				fail("Not enough tokens");
			}
			IToken token = it.next();
			assertEquals("Type mismatch", type, token.getType());
		}

		// make sure that there are no tokens left in the scanner
		assertFalse("More tokens found than expected token types specified",
				it.hasNext());

		int messageCount = logger.getMessages().size();
		assertEquals("Had " + messageCount + " messages. " + expectedMessages
				+ " are expected.", expectedMessages, messageCount);
	}

	/**
	 * Creates a token scope from a directory, i.e. a hierarchy of text
	 * elements/containers. The pattern arrays may be null.
	 */
	protected ITokenResource createTokenScope(File rootDirectory,
			ELanguage language, String[] includePattern, String[] excludePattern)
			throws Exception {
		Object factory = executeProcessor(TokenElementFactory.class,
				"(language=(name=", language.name(), "))");
		IResource resource = createScope(rootDirectory, includePattern,
				excludePattern, factory);
		return (ITokenResource) executeProcessor(TokenResourceSelector.class,
				"(input=(ref=", resource, "))");
	}

	/**
	 * Creates a token resource hierarchy for the given directory of Java files.
	 * This includes all files ending with .java in the specified directory
	 */
	protected ITokenResource createJavaResourceHierarchyFor(File rootDir)
			throws ConQATException {
		return createResourceHierarchyFor(rootDir, ELanguage.JAVA, "**/*.java");
	}

	/** Creates a token resource hierarchy for the given directory of files. */
	protected ITokenResource createResourceHierarchyFor(File rootDir,
			ELanguage language, String... includePatterns)
			throws ConQATException {
		assertTrue("Expected include patterns", includePatterns.length > 0);

		List<String> includeExpressions = new ArrayList<>();
		for (String includePattern : includePatterns) {
			includeExpressions
					.add("include=(pattern='" + includePattern + "')");
		}

		try {
			IContentAccessor[] accessors = (IContentAccessor[]) executeProcessor(
					FileSystemScope.class, "(root=(dir='", rootDir.getPath(),
					"')," + StringUtils.concat(includeExpressions, ", ") + ")");
			return createTokenResourceHierarchy(accessors, language);
		} catch (Exception e) {
			throw new ConQATException(e);
		}
	}

	/**
	 * Creates a token resource hierarchy for all Java files in the given zip
	 * file.
	 */
	protected ITokenResource createJavaResourceHierarchyFromZip(File zipFile)
			throws ConQATException {
		return createResourceHierarchyFromZip(zipFile, ELanguage.JAVA);
	}

	/**
	 * Creates a token resource hierarchy for all files with the given language
	 * in the given zip file. This includes all files with the extensions of the
	 * specified language.
	 */
	protected ITokenResource createResourceHierarchyFromZip(File zipFile,
			ELanguage language) throws ConQATException {
		try {
			IContentAccessor[] zipAccessors = (IContentAccessor[]) executeProcessor(
					SingleFileScope.class, "(file=(path='",
					zipFile.getAbsolutePath(), "'))");
			Object zipFactory = executeProcessor(BinaryElementFactory.class,
					"()");
			IResource zipResource = (IResource) executeProcessor(
					ResourceBuilder.class, "(scope=(ref=", zipAccessors,
					"), factory=(pattern='**', ref=", zipFactory, "))");

			IContentAccessor[] accessors = (IContentAccessor[]) executeProcessor(
					ZipFileScope.class, "('zip-resource'=(ref=", zipResource,
					"),include=(pattern='**'))");

			List<Object> patternArgs = new ArrayList<>();
			for (String fileExtension : language.getFileExtensions()) {
				patternArgs.add("('ant-pattern'=(pattern='**." + fileExtension
						+ "'))");
			}

			PatternList patternList = (PatternList) executeProcessor(
					AntPatternListDef.class,
					CollectionUtils.toArray(patternArgs, Object.class));

			accessors = (IContentAccessor[]) executeProcessor(
					ContentAccessorPathFilter.class, "('scope'=(ref=",
					accessors, "), 'include-patterns'=(ref=", patternList, "))");

			return createTokenResourceHierarchy(accessors, language);
		} catch (Exception e) {
			throw new ConQATException(e);
		}
	}

	/**
	 * Creates a token resource hierarchy for the given content accessors using
	 * the specified language.
	 */
	private ITokenResource createTokenResourceHierarchy(
			IContentAccessor[] accessors, ELanguage language)
			throws ConQATException {
		Object factory = executeProcessor(TokenElementFactory.class,
				"(language=(name=", language, "))");
		IResource resource = (IResource) executeProcessor(
				ResourceBuilder.class, "(scope=(ref=", accessors,
				"), factory=(pattern='**', ref=", factory, "))");
		return (ITokenResource) executeProcessor(TokenResourceSelector.class,
				"(input=(ref=", resource, "))");
	}

	/**
	 * Executes the given processor on the given file and asserts that the value
	 * under the given key in the file matches the expected value.
	 */
	protected <T> void assertKeyValue(
			Class<? extends IConQATProcessor> processor, String filename,
			T expectedValue, String key) throws Exception {
		ITokenElement element = createTokenElement(
				useCanonicalTestFile(filename), ELanguage.JAVA);
		executeProcessor(processor, "(input=(ref=", element, "))");
		Object value = element.getValue(key);
		assertNotNull(value);
		assertTrue("Value should be of class " + expectedValue.getClass(),
				value.getClass().equals(expectedValue.getClass()));
		assertEquals(expectedValue, value);
	}

	/**
	 * Executes the given processor on the given file and asserts that the
	 * returns finding list contains exactly the given findings (serialized as
	 * string).
	 */
	protected void assertFindingList(
			Class<? extends IConQATProcessor> processor, String filename,
			String... expectedFindings) throws Exception {
		assertFindingList(processor, filename, ELanguage.JAVA, expectedFindings);
	}

	/**
	 * Executes the given processor on the given file and asserts that the
	 * returns finding list contains exactly the given findings (serialized as
	 * string).
	 */
	protected void assertFindingList(
			Class<? extends IConQATProcessor> processor, String filename,
			ELanguage language, String... expectedFindings) throws Exception {
		assertFindingList(
				createTokenElement(useCanonicalTestFile(filename), language),
				processor, expectedFindings);
	}

	/**
	 * Executes the given element and asserts that the size of the finding list
	 * value under the given key in the file matches the expected value.
	 */
	private void assertFindingList(ITokenElement element,
			Class<? extends IConQATProcessor> processor,
			String... expectedFindings) throws ConQATException {
		executeProcessor(processor, "(input=(ref=", element, "))");
		assertEquals(StringUtils.concat(expectedFindings, "\n"),
				extractFindingsAsString(element));
	}

	/**
	 * Asserts that the given code results in exactly one finding stored in key
	 * "finding".
	 */
	protected void assertFinding(String code, ELanguage language,
			Class<? extends IConQATProcessor> processor) throws Exception {
		ITokenElement element = createTokenElement(code, language);
		executeProcessor(processor, "(input=(ref=", element, "))");
		FindingsList findingsList = (FindingsList) element.getValue("findings");
		assertTrue(findingsList != null && findingsList.size() == 1);
	}

	/** Asserts that the given code results in no findings. */
	protected void assertNoFinding(String code, ELanguage language,
			Class<? extends IConQATProcessor> processor) throws Exception {
		assertFindingList(createTokenElement(code, language), processor);
	}

	/** Creates a token element from a file. */
	public static ITokenElement createTokenElementFromFile(ELanguage language,
			File file) throws FileNotFoundException, IOException {
		InMemoryContentAccessor accessor = new InMemoryContentAccessor(
				file.getName(), FileSystemUtils.readFileBinary(file));
		return new TokenElement(accessor, FileSystemUtils.UTF8_CHARSET,
				language);
	}

	/**
	 * Checks for incomplete entities, as in this case the cause of a bug is
	 * often not the functionality under test, but rather the parser.
	 */
	public static void checkForIncompleteEntities(ITokenElement element)
			throws ConQATException {
		List<ShallowEntity> entities = ShallowParserFactory.parse(element,
				new LoggerMock());
		ShallowEntity.traverse(entities, new ShallowEntityVisitorBase() {
			@Override
			public boolean visit(ShallowEntity entity) {
				if (!entity.isCompleted()) {
					fail("Found incomplete entity: " + entity);
				}
				return true;
			}
		});
	}

	/**
	 * Parses the given code snippet using the shallow parser of the given
	 * language.
	 */
	public static List<ShallowEntity> parseFragment(String code,
			ELanguage language) throws ConQATException {
		List<IToken> tokens;
		try {
			tokens = scan(code, language);
		} catch (IOException e) {
			throw new AssertionError(
					"Should not happen for in-memory scanning!");
		}
		return ShallowParserFactory.createParser(language)
				.parseFragment(tokens);
	}

	/**
	 * Parses the given code like it's a complete file using the shallow parser
	 * of the given language.
	 */
	public static List<ShallowEntity> parseTopLevel(String code,
			ELanguage language) throws ConQATException {
		List<IToken> tokens;
		try {
			tokens = scan(code, language);
		} catch (IOException e) {
			throw new AssertionError(
					"Should not happen for in-memory scanning!");
		}
		return ShallowParserFactory.createParser(language)
				.parseTopLevel(tokens);
	}

	/**
	 * Scans the given code and returns the generated tokens.
	 */
	public static List<IToken> scan(String code, ELanguage language)
			throws IOException {
		return ScannerUtils.readTokens(ScannerFactory.newLenientScanner(
				language, code, null));
	}

}