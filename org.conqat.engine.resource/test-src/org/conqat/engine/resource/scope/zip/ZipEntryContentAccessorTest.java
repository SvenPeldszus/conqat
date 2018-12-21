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
package org.conqat.engine.resource.scope.zip;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.engine.resource.IContentAccessor;
import org.conqat.engine.resource.base.ContentAccessorBase;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.test.CCSMTestCaseBase;

/**
 * Tests the {@link ZipEntryContentAccessor}.
 * 
 * @author $Author: goeb $
 * @version $Rev: 51153 $
 * @ConQAT.Rating GREEN Hash: 803F86B9973B03910718FA399A0DCF7D
 */
public class ZipEntryContentAccessorTest extends CCSMTestCaseBase {

	/** Zip file for testing purposes. */
	private static final String TEST_ZIP = "files.zip";

	/** Case-sensitive Zip file for testing purposes. */
	private static final String CASE_SENSITIVE_TEST_ZIP = "case_sensitive.zip";

	/** Tests content reading. */
	public void testReadContent() throws ConQATException {
		IContentAccessor a = new ZipEntryContentAccessor(
				useCanonicalTestFile(TEST_ZIP), "b/b.txt",
				StringUtils.EMPTY_STRING, "foo", true, null);
		assertEquals("b*2", new String(a.getContent()));
	}

	/** Tests the creation of relative paths. */
	public void testCreateRelative() throws ConQATException {
		IContentAccessor a = new ZipEntryContentAccessor(
				useCanonicalTestFile(TEST_ZIP), "a.txt",
				StringUtils.EMPTY_STRING, "TEST", true, null);
		assertEquals("TEST/a.txt", a.getUniformPath());

		IContentAccessor b = assertRelativeWithContent(a, "b/b.txt",
				"TEST/b/b.txt", "b*2");

		// this should also work with the backslash (even on linux)
		assertRelativeWithContent(a, "b\\b.txt", "TEST/b/b.txt", "b*2");

		// file must exist in order to be accessible
		try {
			a.createRelative("does/not/exist");
			fail("expected exception!");
		} catch (ConQATException e) {
			// expected
		}

		// we can even resolve .. to some extent
		assertRelativeWithContent(b, "../long_name/b.txt",
				"TEST/long_name/b.txt", "b*2");
	}

	/**
	 * Tests the creation of relative paths with an entry prefix. The entry
	 * prefix must not be part of relative names.
	 */
	public void testCreateRelativeWithPrefix() throws ConQATException {
		IContentAccessor b_b_txt = new ZipEntryContentAccessor(
				useCanonicalTestFile(TEST_ZIP), "b/b.txt", "b", "TEST", true,
				null);
		assertEquals("TEST/b.txt", b_b_txt.getUniformPath());

		IContentAccessor b_c_a_txt = assertRelativeWithContent(b_b_txt,
				"c/a.txt", "TEST/c/a.txt", "a");
		assertRelativeWithContent(b_c_a_txt, "../b.txt", "TEST/b.txt", "b*2");

		// We cannot access a file outside the root of a uniform path.
		try {
			b_b_txt.createRelativeUniformPath("../a.txt");
			fail("Expected a ConQATException to be thrown.");
		} catch (ConQATException e) {
			assertEquals(
					"The relative path '../a.txt' is not located under the root of the uniform path.",
					e.getMessage());
		}
	}

	/**
	 * Asserts that creating a relative content accessor has a given uniform
	 * path an content.
	 * 
	 * @return the relative content accessor.
	 */
	private IContentAccessor assertRelativeWithContent(
			IContentAccessor accessor, String relativePath, String uniformPath,
			String content) throws ConQATException {
		assertEquals(uniformPath,
				accessor.createRelativeUniformPath(relativePath));
		IContentAccessor relative = accessor.createRelative(relativePath);
		assertEquals(uniformPath, relative.getUniformPath());
		assertEquals(content, new String(relative.getContent()));

		return relative;
	}

	/** Tests the case-insensitive reading of a zip file. */
	public void testCaseInsensitiveReading() throws ConQATException {
		ContentAccessorBase a = new ZipEntryContentAccessor(
				useCanonicalTestFile(CASE_SENSITIVE_TEST_ZIP), "a.txt",
				StringUtils.EMPTY_STRING, "TEST", false, null);

		IContentAccessor g = a.createRelative("a/Cd/a.TXT");
		assertEquals("TEST/A/cD/a.txt", g.getUniformPath());
		assertEquals("a", new String(g.getContent()));

		// file must exist in order to be accessible
		try {
			a.createRelative("Does/noT/b.tXt");
			fail("expected exception!");
		} catch (ConQATException e) {
			// expected
		}

		// test relative resolving
		IContentAccessor b2 = g.createRelative("../B.tXt");
		assertEquals("TEST/A/b.TXT", b2.getUniformPath());
	}

	/** tests content accessor equality for case insensitivity. */
	public void testEquals() throws ConQATException {
		ContentAccessorBase lowerA = new ZipEntryContentAccessor(
				useCanonicalTestFile(CASE_SENSITIVE_TEST_ZIP), "a.txt",
				StringUtils.EMPTY_STRING, "TEST", false, null);

		ContentAccessorBase upperA = new ZipEntryContentAccessor(
				useCanonicalTestFile(CASE_SENSITIVE_TEST_ZIP), "A.txt",
				StringUtils.EMPTY_STRING, "TEST", false, null);

		ContentAccessorBase upperSensitiveA = new ZipEntryContentAccessor(
				useCanonicalTestFile(CASE_SENSITIVE_TEST_ZIP), "A.txt",
				StringUtils.EMPTY_STRING, "TEST", true, null);

		ContentAccessorBase someB = new ZipEntryContentAccessor(
				useCanonicalTestFile(CASE_SENSITIVE_TEST_ZIP), "a/b.txt",
				StringUtils.EMPTY_STRING, "TEST", false, null);

		assertTrue(lowerA.equals(upperA));
		assertFalse(lowerA.equals(upperSensitiveA));
		assertFalse(lowerA.equals(someB));
	}
}