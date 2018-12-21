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
package org.conqat.engine.sourcecode.shallowparser.languages.cpp;

import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_METHOD;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.IN_TYPE;
import static org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates.TOP_LEVEL;
import static org.conqat.lib.scanner.ETokenType.CLASS;
import static org.conqat.lib.scanner.ETokenType.ENUM;
import static org.conqat.lib.scanner.ETokenType.EXTERN;
import static org.conqat.lib.scanner.ETokenType.GT;
import static org.conqat.lib.scanner.ETokenType.IDENTIFIER;
import static org.conqat.lib.scanner.ETokenType.LBRACE;
import static org.conqat.lib.scanner.ETokenType.LPAREN;
import static org.conqat.lib.scanner.ETokenType.LT;
import static org.conqat.lib.scanner.ETokenType.MULT;
import static org.conqat.lib.scanner.ETokenType.PREPROCESSOR_DIRECTIVE;
import static org.conqat.lib.scanner.ETokenType.PREPROCESSOR_INCLUDE;
import static org.conqat.lib.scanner.ETokenType.RBRACE;
import static org.conqat.lib.scanner.ETokenType.RPAREN;
import static org.conqat.lib.scanner.ETokenType.SEMICOLON;
import static org.conqat.lib.scanner.ETokenType.STRUCT;
import static org.conqat.lib.scanner.ETokenType.TEMPLATE;
import static org.conqat.lib.scanner.ETokenType.UNION;
import static org.conqat.lib.scanner.ETokenType.USING;

import java.util.EnumSet;

import org.conqat.engine.sourcecode.shallowparser.framework.EShallowEntityType;
import org.conqat.engine.sourcecode.shallowparser.framework.RecognizerBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.CStyleShallowParserRuleProviderBase;
import org.conqat.engine.sourcecode.shallowparser.languages.base.EGenericParserStates;

/**
 * Provides the meta rules for the {@link CppShallowParser}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: 54A56B9D577997E7851C76B50CAC4E2F
 */
/* package */class CppShallowParserMetaRules extends
		CStyleShallowParserRuleProviderBase<CppShallowParser> {

	/** Constructor. */
	public CppShallowParserMetaRules(CppShallowParser delegateParser) {
		super(delegateParser);
	}

	/** {@inheritDoc} */
	@Override
	public void contributeRules() {
		// preprocessor directives
		inAnyState()
				.sequence(
						EnumSet.of(PREPROCESSOR_DIRECTIVE, PREPROCESSOR_INCLUDE))
				.createNode(EShallowEntityType.META, 0).endNode();

		// template declaration
		inAnyState().sequence(TEMPLATE).createNode(EShallowEntityType.META, 0)
				.skipNested(LT, GT).endNode();

		// forward declarations
		inState(TOP_LEVEL, IN_TYPE)
				.sequence(EnumSet.of(CLASS, STRUCT, ENUM, UNION))
				.subRecognizer(delegateParser.createScopeRecognizer(), 0,
						Integer.MAX_VALUE).sequence(IDENTIFIER)
				.sequence(SEMICOLON)
				.createNode(EShallowEntityType.META, "forward declaration", -2)
				.endNode();

		// friend
		inAnyState().sequence(USING).createNode(EShallowEntityType.META, 0)
				.skipTo(SEMICOLON).endNode();

		createExternCRule();
		createAssemblyRules();
	}

	/** Creates the rule for the "extern C" construct. */
	private void createExternCRule() {
		RecognizerBase<EGenericParserStates> externCAlternative = inState(
				TOP_LEVEL).sequence(EXTERN, new CStringMatcher());
		externCAlternative.sequence(LBRACE)
				.createNode(EShallowEntityType.META, "extern C block")
				.parseUntil(TOP_LEVEL).sequence(RBRACE).endNode();
		externCAlternative.createNode(EShallowEntityType.META,
				"extern C prefix").endNode();
	}

	/** Create rules for inline assembly blocks. */
	private void createAssemblyRules() {
		inState(IN_METHOD).sequence(new ASMIdentifierMatcher(), LBRACE)
				.createNode(EShallowEntityType.META, "inline assembler block")
				.skipTo(RBRACE).endNode();
		inState(TOP_LEVEL)
				.sequence(new ASMIdentifierMatcher(),
						CppShallowParser.PRIMITIVE_TYPES)
				.optional(MULT)
				.markStart()
				.sequence(IDENTIFIER, LPAREN)
				.createNode(EShallowEntityType.METHOD,
						"inline assembler function", 0).skipTo(RPAREN)
				.skipTo(LBRACE).skipTo(RBRACE).endNode();
	}
}
