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
package org.conqat.engine.dotnet.ila;

import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATFieldParameter;
import org.conqat.engine.core.core.AConQATKey;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.AConQATProcessor;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.dotnet.ila.xml.IlaXmlReader;
import org.conqat.engine.resource.text.ITextElement;

/**
 * {@ConQAT.Doc}
 * 
 * @see ILAnalyzerRunnerProcessor
 * 
 * @author $Author: streitel $
 * @version $Revision: 50996 $
 * @ConQAT.Rating RED Hash: F11F4186CDBA3525C721331454E2022C
 */
@AConQATProcessor(description = "Reads dependency information stored in XML produced by the Intermediate "
		+ "Language Analyzer files into a representation that can be used for "
		+ "architecture assessment. "
		+ "In contrast to Java, where class files closely resemble package structure, "
		+ "the mapping between code and assemblies is a lot more arbitrary in the .NET "
		+ "world. In order to map types found in IL to source files, debug information "
		+ "is required that is not always available. "
		+ "This processor is hence NO pipeline processor that annotates a source tree "
		+ "with dependency information. Instead, it parses all ILA XML files located "
		+ "in a resource hierarchy and returns a very simple rooted list "
		+ "of types and dependencies that can be used to create a dependency graph.")
public class ILDependenciesImporterProcessor extends ILImporterProcessorBase {

	/** Key under which members are stored */
	@AConQATKey(description = "Key under which the members of the analyzed type are stored", type = "java.util.List<String>")
	public static final String MEMBERS = "Members";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the metadata token is stored. The metadata token identifies a member in a type.", type = "java.lang.String")
	public static final String TOKEN = "Token";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the name is stored.", type = "java.lang.String")
	public static final String NAME = "Name";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the raw name is stored.", type = "java.lang.String")
	// TODO (FS) please document what a "raw name" is. this may not be obvious
	// to someone using the conqat UI
	public static final String RAW_NAME = "Raw Name";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the type is stored.", type = "java.lang.String")
	public static final String TYPE = "Type";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the declaration type (class, interface, ...) is stored.", type = "java.lang.String")
	public static final String DECLTYPE = "DeclType";

	/** {@ConQAT.Doc} */
	@AConQATKey(description = "Key under which the number of IL statement is stored.", type = "int")
	public static final String IL_STATEMENT_COUNT = "IlStatementCount";

	/** Dependencies in types whose base type matches are ignored */
	private PatternList ignoreBaseTypePatterns = new PatternList();

	/**
	 * Flag that determines whether to ignore types generated by the compiler.
	 * Default value is true.
	 */
	private boolean ignoreSynthetic = true;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "ignore-basetype", description = "If set, types whose base class matches one of these patters are ignored", minOccurrences = 0, maxOccurrences = 1)
	public void setIgnoreBaseTypePatterns(
			@AConQATAttribute(name = "patterns", description = "Regular expressions") PatternList ignoreBaseTypePatterns) {
		this.ignoreBaseTypePatterns = ignoreBaseTypePatterns;
	}

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = "ignore-synthetic", description = "Flag that determines whether to ignore types generated by the compiler. Default value is true.", minOccurrences = 0, maxOccurrences = 1)
	public void setIgnoreSynthetic(
			@AConQATAttribute(name = "value", description = "Default value is true") boolean ignoreSynthetic) {
		this.ignoreSynthetic = ignoreSynthetic;
	}

	/** {@ConQAT.Doc} */
	@AConQATFieldParameter(parameter = "method-body", attribute = "include", optional = true, description = "Flag that determines whether the method body gets read. Setting this to true will heavily impact the memory footprint. Default is false.")
	public boolean includeMethodBody = false;

	/** {@inheritDoc} */
	@Override
	protected String[] getKeys() {
		return new String[] { MEMBERS, DECLTYPE, IL_STATEMENT_COUNT };
	}

	/** Create actual XML reader */
	@Override
	protected IlaXmlReader createXmlReader(ITextElement element)
			throws ConQATException {
		return new IlaXmlReader(element, outputRoot, excludePatterns,
				includePatterns, ignoreBaseTypePatterns, ignoreSynthetic,
				includeMethodBody, includedDependencies, excludedDependencies);
	}

}