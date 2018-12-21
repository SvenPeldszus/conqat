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
package org.conqat.engine.resource.diff;

import java.util.Set;

import org.conqat.engine.commons.pattern.PatternTransformationList;
import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.core.logging.testutils.LoggerMock;
import org.conqat.engine.resource.IResource;
import org.conqat.engine.resource.build.ResourceBuilder;
import org.conqat.engine.resource.test.ResourceProcessorTestCaseBase;
import org.conqat.engine.resource.text.ITextElement;
import org.conqat.engine.resource.text.ITextResource;
import org.conqat.engine.resource.text.TextResourceSelector;
import org.conqat.lib.commons.clone.DeepCloneException;
import org.conqat.lib.commons.collections.Pair;
import org.conqat.lib.commons.string.RegexReplacement;

/**
 * Test for {@link ScopeDiffInfo}.
 * 
 * @author $Author: deissenb $
 * @version $Rev: 48648 $
 * @ConQAT.Rating YELLOW Hash: 9CF212FB97F7C7DFD67A1D073E6BCEDC
 */
public class ScopeDiffInfoTest extends ResourceProcessorTestCaseBase {

	/** Test scopes without modification. */
	public void testUnmodified() throws ConQATException {

		ScopeDiffInfo info = createScopeDiffInfo(
				"'TEST/a/b.txt'=B, 'TEST/a/c.txt'=C, 'TEST/d.txt'=D, 'TEST/x/y/z.txt'=E",
				"'TEST/a/b.txt'=B, 'TEST/a/c.txt'=C, 'TEST/d.txt'=D, 'TEST/x/y/z.txt'=E");

		assertEquals(0, info.getModifiedElements().size());
		assertEquals(0, info.getRemovedElements().size());
		assertEquals(0, info.getAddedElements().size());
		assertEquals(4, info.getUnmodifiedElements().size());
	}

	/** Test scopes with modified file. */
	public void testModification() throws ConQATException {

		ScopeDiffInfo info = createScopeDiffInfo(
				"'TEST/a/b.txt'=B, 'TEST/a/c.txt'=C, 'TEST/d.txt'=D, 'TEST/x/y/z.txt'=E",
				"'TEST-BL/a/b.txt'=B, 'TEST-BL/a/c.txt'=C, 'TEST-BL/d.txt'=D, 'TEST-BL/x/y/z.txt'=New");

		assertEquals(1, info.getModifiedElements().size());
		assertContains(info.getModifiedElements(), "TEST/x/y/z.txt");

		assertEquals(0, info.getRemovedElements().size());
		assertEquals(0, info.getAddedElements().size());
		assertEquals(3, info.getUnmodifiedElements().size());
	}

	/** Test addition. */
	public void testAdd() throws ConQATException {

		ScopeDiffInfo info = createScopeDiffInfo(
				"'TEST/a/b.txt'=B, 'TEST/a/c.txt'=C, 'TEST/d.txt'=D, 'TEST/x/y/z.txt'=E, 'TEST/a/new.txt'=F",
				"'TEST-BL/a/b.txt'=B, 'TEST-BL/a/c.txt'=C, 'TEST-BL/d.txt'=D, 'TEST-BL/x/y/z.txt'=E");

		assertEquals(0, info.getModifiedElements().size());

		assertEquals(0, info.getRemovedElements().size());
		assertEquals(1, info.getAddedElements().size());
		assertContains(info.getAddedElements(), "TEST/a/new.txt");
		assertEquals(4, info.getUnmodifiedElements().size());
	}

	/** Test removal. */
	public void testRemove() throws ConQATException {

		ScopeDiffInfo info = createScopeDiffInfo(
				"'TEST/a/b.txt'=B, 'TEST/a/c.txt'=C, 'TEST/d.txt'=D",
				"'TEST-BL/a/b.txt'=B, 'TEST-BL/a/c.txt'=C, 'TEST-BL/d.txt'=D, 'TEST-BL/x/y/z.txt'=E");

		assertEquals(0, info.getModifiedElements().size());

		assertEquals(1, info.getRemovedElements().size());
		assertContains(info.getRemovedElements(), "TEST-BL/x/y/z.txt");

		assertEquals(0, info.getAddedElements().size());
		assertEquals(3, info.getUnmodifiedElements().size());
	}

	/** Test multiple changes. */
	public void testMultipleChanges() throws ConQATException {

		ScopeDiffInfo info = createScopeDiffInfoWithMultipleChanges();

		assertEquals(1, info.getModifiedElements().size());
		assertContains(info.getModifiedElements(), "TEST/x/y/z.txt");

		assertEquals(1, info.getRemovedElements().size());
		assertContains(info.getRemovedElements(), "TEST-BL/a/c.txt");

		assertEquals(1, info.getAddedElements().size());
		assertContains(info.getAddedElements(), "Test/a/new.txt");

		assertEquals(2, info.getUnmodifiedElements().size());
	}

	/** Test {@link ScopeDiffInfo#getElements(String)}. */
	public void testGetElements() throws ConQATException {
		ScopeDiffInfo info = createScopeDiffInfoWithMultipleChanges();

		// present in both
		Pair<ITextElement, ITextElement> elements = info
				.getElements("TEST/a/b.txt");
		assertEquals("TEST/a/b.txt", elements.getFirst().getUniformPath());
		assertEquals("TEST-BL/a/b.txt", elements.getSecond().getUniformPath());

		// new
		elements = info.getElements("Test/a/new.txt");
		assertEquals("Test/a/new.txt", elements.getFirst().getUniformPath());
		assertNull(elements.getSecond());

		// removed
		elements = info.getElements("TEST-BL/a/c.txt");
		assertNull(elements.getFirst());
		assertEquals("TEST-BL/a/c.txt", elements.getSecond().getUniformPath());
	}

	/** Test deep cloning. */
	public void testDeepCloning() throws ConQATException, DeepCloneException {
		ScopeDiffInfo info = createScopeDiffInfoWithMultipleChanges();
		ScopeDiffInfo clone = info.deepClone();

		Pair<ITextElement, ITextElement> elements = info
				.getElements("Test/a/new.txt");
		elements.getFirst().setValue("deep-clone-check", true);

		Pair<ITextElement, ITextElement> clonedElements = clone
				.getElements("Test/a/new.txt");
		assertNull("Value should not be present", clonedElements.getFirst()
				.getValue("deep-clone-check"));

	}

	/** Create a scope diff with multiple changes. */
	private ScopeDiffInfo createScopeDiffInfoWithMultipleChanges()
			throws ConQATException {
		ScopeDiffInfo info = createScopeDiffInfo(
				"'TEST/a/b.txt'=B,  'TEST/d.txt'=D, 'TEST/x/y/z.txt'=E, 'Test/a/new.txt'=F",
				"'TEST-BL/a/b.txt'=B, 'TEST-BL/a/c.txt'=C, 'TEST-BL/d.txt'=D, 'TEST-BL/x/y/z.txt'=New");
		return info;
	}

	/** Assert that a set contains a specific element. */
	private void assertContains(Set<ITextElement> elements, String uniformPath) {
		for (ITextElement element : elements) {
			if (uniformPath.equals(element.getUniformPath())) {
				return;
			}
		}
		fail(uniformPath + " not found");
	}

	/** Create scope diff info. */
	private ScopeDiffInfo createScopeDiffInfo(String mainDescription,
			String compareeDescription) throws ConQATException {
		ITextResource mainRoot = createTextScope(mainDescription);
		ITextResource compareeRoot = createTextScope(compareeDescription);

		PatternTransformationList transformations = new PatternTransformationList();

		transformations.add(new RegexReplacement("TEST-BL", "TEST"));

		return new ScopeDiffInfo(mainRoot, compareeRoot, transformations,
				new LoggerMock());
	}

	/** Create a text scope. */
	private ITextResource createTextScope(String description)
			throws ConQATException {
		IResource input = (IResource) executeProcessor(ResourceBuilder.class,
				"(scope=(ref=memScope(" + description + ")), ",
				"factory=(pattern='**/*.txt', ref=textFactory()))");
		return (ITextResource) executeProcessor(TextResourceSelector.class,
				"(input=(ref=", input, "))");

	}

}
