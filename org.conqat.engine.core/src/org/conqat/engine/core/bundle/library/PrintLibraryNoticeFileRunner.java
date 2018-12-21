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
package org.conqat.engine.core.bundle.library;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Comparator;

import org.conqat.lib.commons.collections.CollectionUtils;
import org.conqat.lib.commons.collections.UnmodifiableList;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.options.AOption;
import org.conqat.lib.commons.string.StringUtils;

/**
 * Prints notice information about the 3rd party libraries in the loaded
 * bundles.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47490 $
 * @ConQAT.Rating GREEN Hash: 186C4D972002C76B3DA4021A8BF59F6A
 */
public class PrintLibraryNoticeFileRunner extends LibraryDescriptorRunnerBase {

	/**
	 * Compares library descriptors based on their lowercase name, i.e. sorts
	 * them alphabetically.
	 */
	private static final Comparator<LibraryDescriptor> LIBRARY_DESRIPTOR_COMPARATOR = new Comparator<LibraryDescriptor>() {

		@Override
		public int compare(LibraryDescriptor first, LibraryDescriptor second) {
			return first.getName().toLowerCase()
					.compareTo(second.getName().toLowerCase());
		}
	};
	/** An optional preamble to be added to the notice information. */
	private String preamble = null;

	/**
	 * Adds a preamble to the notice information, which is specified in the
	 * given file.
	 */
	@AOption(shortName = 'p', longName = "preamble", description = "Adds the preamble in the specified file to the notice information.")
	public void setPreamble(String fileName) throws IOException {
		String content = FileSystemUtils.readFile(new File(fileName));
		preamble = StringUtils.normalizeLineBreaks(content);
	}

	/** {@inheritDoc} */
	@Override
	protected void doRun(PrintStream out) {
		if (preamble != null) {
			out.println(preamble);
		}

		UnmodifiableList<LibraryDescriptor> libraryDescriptors = CollectionUtils
				.asSortedUnmodifiableList(getLibraryDescriptors(),
						LIBRARY_DESRIPTOR_COMPARATOR);
		for (LibraryDescriptor descriptor : libraryDescriptors) {
			printNoticeForDescriptor(out, descriptor);
		}
	}

	/** Prints notice information for the given library descriptor. */
	private void printNoticeForDescriptor(PrintStream out,
			LibraryDescriptor descriptor) {

		out.println();
		out.println(StringUtils.fillString(80, '-'));
		out.println();

		out.println("The " + descriptor.getBundle().getName() + " bundle uses "
				+ descriptor.getName());
		out.println(" (" + descriptor.getWebsite() + "),");
		out.println("licensed under " + descriptor.getLicense().getName());
		out.println(" (" + descriptor.getLicense().getWebsite() + ").");

		String notice = descriptor.getLicense().getNotice();
		if (!StringUtils.isEmpty(notice)) {
			out.println();
			out.println(notice);
		}

	}
}
