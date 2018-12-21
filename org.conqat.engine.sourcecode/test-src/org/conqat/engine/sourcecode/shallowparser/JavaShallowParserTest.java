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
package org.conqat.engine.sourcecode.shallowparser;

import org.conqat.engine.sourcecode.shallowparser.languages.java.JavaShallowParser;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link JavaShallowParser}.
 *
 * @author $Author: hummelb $
 * @version $Rev: 51186 $
 * @ConQAT.Rating GREEN Hash: 42D86FF5A5EE5F0EDB8C4640CFB7BDFB
 */
public class JavaShallowParserTest extends CStyleShallowParserTestBase {

	/** Tests parsing of method fragments. */
	public void testMethodFragments() {
		assertFragmentParsedTo(
				"public boolean foo (int my) { dosomething(); }",
				"METHOD: method: foo (lines 1-1)\n"
						+ "  STATEMENT: simple statement: dosomething (lines 1-1)\n");
	}

	/** Tests parsing of do...while loops. */
	public void testDoWhileFragments() {
		assertFragmentParsedTo("do { a = a + 1; } while (a < 10); print(a);",
				"STATEMENT: do: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: a (lines 1-1)\n"
						+ "STATEMENT: simple statement: print (lines 1-1)\n");
	}

	/** Tests parsing of do...while loops. */
	public void testPackageFragments() {
		assertFragmentParsedTo("package foo.bar.is.awesome;",
				"META: package: foo.bar.is.awesome (lines 1-1)\n");
	}

	/** {@inheritDoc} */
	@Override
	protected ELanguage getLanguage() {
		return ELanguage.JAVA;
	}
}
