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
package org.conqat.engine.sourcecode.analysis;

import java.util.List;

import org.conqat.engine.commons.ConQATParamDoc;
import org.conqat.engine.commons.pattern.PatternList;
import org.conqat.engine.core.core.AConQATAttribute;
import org.conqat.engine.core.core.AConQATParameter;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.core.IConQATParameterObject;
import org.conqat.engine.core.logging.IConQATLogger;
import org.conqat.engine.resource.util.ResourceTraversalUtils;
import org.conqat.engine.sourcecode.resource.ITokenElement;
import org.conqat.engine.sourcecode.resource.ITokenResource;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.scanner.ETokenType;
import org.conqat.lib.scanner.ETokenType.ETokenClass;
import org.conqat.lib.scanner.IToken;

/**
 * Support class for processors that analyze literals.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49409 $
 * @ConQAT.Rating GREEN Hash: 0D90AEFA685FCFCD6228B719983D1991
 */
public abstract class LiteralAnalyzerSupport implements IConQATParameterObject {

	/** Exclude patterns. */
	private PatternList excludePatterns;

	/** {@ConQAT.Doc} */
	@AConQATParameter(name = ConQATParamDoc.EXCLUDE_NAME, maxOccurrences = 1, description = ""
			+ "Patterns for literals to be excluded. "
			+ "Only literals matching the entire pattern are excluded.")
	public void setExcludePattern(
			@AConQATAttribute(name = ConQATParamDoc.PATTERN_LIST, description = ConQATParamDoc.PATTERN_LIST_DESC) PatternList excludePatterns) {
		this.excludePatterns = excludePatterns;
	}

	/**
	 * Analyze all literals in all elements of the resource tree. For each
	 * non-excluded literal {@link #processLiteral(IToken, ITokenElement)} is
	 * called.
	 * 
	 * @throws ConQATException
	 *             if I/O problems occur
	 */
	public void processLiterals(ITokenResource input, IConQATLogger logger)
			throws ConQATException {
		List<ITokenElement> elements = ResourceTraversalUtils.listElements(
				input, ITokenElement.class);

		for (ITokenElement element : elements) {
			UnmodifiableList<IToken> tokens = element.getTokens(logger);
			for (IToken token : tokens) {
				if (token.getType().getTokenClass() == ETokenClass.LITERAL
						&& !isExcluded(token)) {
					processLiteral(token, element);
				}
			}
		}
	}

	/**
	 * Ignore boolean literals, null literals and literals matched by one of the
	 * ignore patterns.
	 */
	protected boolean isExcluded(IToken token) {
		if (token.getType() == ETokenType.NULL_LITERAL) {
			return true;
		}
		if (token.getType() == ETokenType.BOOLEAN_LITERAL) {
			return true;
		}
		if (token.getType() == ETokenType.CLASS_LITERAL) {
			return true;
		}
		if (excludePatterns != null
				&& excludePatterns.matchesAny(token.getText())) {
			return true;
		}
		return false;
	}

	/** Processes the literal of a token element. */
	public abstract void processLiteral(IToken token, ITokenElement element);
}