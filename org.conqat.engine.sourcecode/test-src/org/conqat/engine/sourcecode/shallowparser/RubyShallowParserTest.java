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

import org.conqat.engine.sourcecode.shallowparser.languages.ruby.RubyShallowParser;
import org.conqat.lib.scanner.ELanguage;

/**
 * Tests the {@link RubyShallowParser}.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 49235 $
 * @ConQAT.Rating GREEN Hash: A103CCCFA72D9F659C899CF5FBFCAB7D
 */
public class RubyShallowParserTest extends ShallowParserTestBase {

	/** Tests parsing of fragments. */
	public void testFragments() {
		assertFragmentParsedTo("def foo\nputs 12\nend",
				"METHOD: method: foo (lines 1-3)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n");
		assertFragmentParsedTo("def foo\nputs 12;\nend",
				"METHOD: method: foo (lines 1-3)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n");
		assertFragmentParsedTo("class a\ndef foo\nputs 12\nend\nend",
				"TYPE: class: a (lines 1-5)\n"
						+ "  METHOD: method: foo (lines 2-4)\n"
						+ "    STATEMENT: simple statement: puts (lines 3-3)\n");
		assertFragmentParsedTo("class a; def foo; puts 12 end end",
				"TYPE: class: a (lines 1-1)\n"
						+ "  METHOD: method: foo (lines 1-1)\n"
						+ "    STATEMENT: simple statement: puts (lines 1-1)\n");
		assertFragmentParsedTo("if foo\nputs 12\nend",
				"STATEMENT: if: null (lines 1-3)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n");
		assertFragmentParsedTo("if foo\nputs 12\nelse\nputs 13\nend",
				"STATEMENT: if: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n"
						+ "STATEMENT: else: null (lines 3-5)\n"
						+ "  STATEMENT: simple statement: puts (lines 4-4)\n");

		assertFragmentParsedTo(
				"if foo\nputs 12\nelsif bar then puts 12.5 else\nputs 13\nend",
				"STATEMENT: if: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n"
						+ "STATEMENT: elsif: null (lines 3-3)\n"
						+ "  STATEMENT: simple statement: puts (lines 3-3)\n"
						+ "STATEMENT: else: null (lines 3-5)\n"
						+ "  STATEMENT: simple statement: puts (lines 4-4)\n");

		assertFragmentParsedTo("while (42 == theanswer) \n foo; \n end",
				"STATEMENT: while: null (lines 1-3)\n"
						+ "  STATEMENT: simple statement: foo (lines 2-2)\n");

		assertFragmentParsedTo("while (42 == theanswer) do foo; \n end",
				"STATEMENT: while: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: foo (lines 1-1)\n");

		assertFragmentParsedTo("until (42 != theanswer) do bar; \n end",
				"STATEMENT: until: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: bar (lines 1-1)\n");

		assertFragmentParsedTo(
				"for kwikemart in 1..apu do callHomer() end",
				"STATEMENT: for: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: callHomer (lines 1-1)\n");

		assertFragmentParsedTo("begin moe+=1; end",
				"STATEMENT: begin: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: moe (lines 1-1)\n");

		assertFragmentParsedTo("begin \nputs 12\nend",
				"STATEMENT: begin: null (lines 1-3)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n");
		assertFragmentParsedTo("begin \nputs 12\nrescue\nputs 13\nend",
				"STATEMENT: begin: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n"
						+ "STATEMENT: rescue: null (lines 3-5)\n"
						+ "  STATEMENT: simple statement: puts (lines 4-4)\n");

		assertFragmentParsedTo(
				"begin \nputs 12\nrescue \nputs 12.5 ensure\nputs 13\nend",
				"STATEMENT: begin: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n"
						+ "STATEMENT: rescue: null (lines 3-4)\n"
						+ "  STATEMENT: simple statement: puts (lines 4-4)\n"
						+ "STATEMENT: ensure: null (lines 4-6)\n"
						+ "  STATEMENT: simple statement: puts (lines 5-5)\n");

		assertFragmentParsedTo(
				"begin \nputs 12\nrescue  Exception => a \nputs 12.5 ensure\nputs 13\nend",
				"STATEMENT: begin: null (lines 1-2)\n"
						+ "  STATEMENT: simple statement: puts (lines 2-2)\n"
						+ "STATEMENT: rescue: null (lines 3-4)\n"
						+ "  STATEMENT: simple statement: puts (lines 4-4)\n"
						+ "STATEMENT: ensure: null (lines 4-6)\n"
						+ "  STATEMENT: simple statement: puts (lines 5-5)\n");

		assertFragmentParsedTo("a = x.map do 1 end",
				"STATEMENT: simple statement: a (lines 1-1)\n"
						+ "  METHOD: block: null (lines 1-1)\n"
						+ "    STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo("a = x.map { 1 }",
				"STATEMENT: simple statement: a (lines 1-1)\n"
						+ "  METHOD: block: null (lines 1-1)\n"
						+ "    STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo("while x.map { 1 }.max < 15 do 1 end",
				"STATEMENT: while: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo("while (x.map do 1 end.max < 15) do 1 end",
				"STATEMENT: while: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo("a = x.map do 1 end if a.empty?",
				"STATEMENT: simple statement: a (lines 1-1)\n"
						+ "  METHOD: block: null (lines 1-1)\n"
						+ "    STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo(
				"while (x.map do 1 end.max < 15) do 1 end unless condition",
				"STATEMENT: while: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: 1 (lines 1-1)\n");

		assertFragmentParsedTo("if true then foo end unless condition",
				"STATEMENT: if: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: foo (lines 1-1)\n");

		assertFragmentParsedTo("foo(x) if x > 12 for x in 0..12",
				"STATEMENT: simple statement: foo (lines 1-1)\n");

		assertFragmentParsedTo(
				"class a\nclass << self\ndef foo\nputs 12\nend\nend\nend",
				"TYPE: class: a (lines 1-7)\n"
						+ "  META: static declarations: null (lines 2-6)\n"
						+ "    METHOD: method: foo (lines 3-5)\n"
						+ "      STATEMENT: simple statement: puts (lines 4-4)\n");

		assertFragmentParsedTo("class a\ndef self.foo\nputs 12\nend\nend",
				"TYPE: class: a (lines 1-5)\n"
						+ "  METHOD: method: self.foo (lines 2-4)\n"
						+ "    STATEMENT: simple statement: puts (lines 3-3)\n");

		assertFragmentParsedTo("a = \"#{foo}\"",
				"STATEMENT: simple statement: a (lines 1-1)\n");

		assertFragmentParsedTo("a = \"#{foo if true}\"",
				"STATEMENT: simple statement: a (lines 1-1)\n");

		assertFragmentParsedTo("a = \"#{if a then a else b end}\"",
				"STATEMENT: simple statement: a (lines 1-1)\n");

		assertFragmentParsedTo(
				"a = \"#{class A; def foo; 12; end end A.new.foo}\"",
				"STATEMENT: simple statement: a (lines 1-1)\n");

		assertFragmentParsedTo("a = 12 if a == \"#{if a then a else b end}\"",
				"STATEMENT: simple statement: a (lines 1-1)\n");

		assertFragmentParsedTo(
				"while a == \"#{if a then a else b end}\" do 1 end",
				"STATEMENT: while: null (lines 1-1)\n"
						+ "  STATEMENT: simple statement: 1 (lines 1-1)\n");

		// assertFragmentParsedTo("a = if a then 12 else b end",
		// "STATEMENT: simple statement: a (lines 1-1)\n");
	}

	/** {@inheritDoc} */
	@Override
	protected ELanguage getLanguage() {
		return ELanguage.RUBY;
	}
}
