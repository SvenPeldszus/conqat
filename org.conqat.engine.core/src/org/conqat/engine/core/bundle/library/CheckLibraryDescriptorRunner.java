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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.conqat.lib.commons.assertion.CCSMAssert;
import org.conqat.lib.commons.assessment.ETrafficLightColor;
import org.conqat.lib.commons.assessment.RatingUtils;
import org.conqat.lib.commons.filesystem.FileSystemUtils;
import org.conqat.lib.commons.options.AOption;
import org.conqat.lib.commons.string.StringUtils;

/**
 * ConQAT runner that validates library descriptors with the following checks:
 * 
 * <ul>
 * <li>(error) Coverage of libraries by descriptor files.
 * <li>(error) Correctness of library descriptors, i.e. all mandatory fields are
 * present.
 * <li>(error) Uniqueness of library descriptors, i.e. no library is covered by
 * more than one descriptor.
 * <li>(error) Usage of a library in an open source release, i.e. compatibility
 * with Apache 2.0.</li>
 * <li>(warn) Usage of a library in a commercial release.</li>
 * <li>(warn) Review status of library descriptors, i.e. non-GREEN descriptor
 * files.</li>
 * </ul>
 * 
 * Checks that are marked with (error) will cause the runner to exit with an
 * error code. Checks that are marked with (warn) will not produce an error code
 * unless forced with a commandline option.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 47490 $
 * @ConQAT.Rating GREEN Hash: CF1A02126C58CB335B7F0D6508F4E1F5
 */
public class CheckLibraryDescriptorRunner extends LibraryDescriptorRunnerBase {

	/** Flag to return an error code for commercial use violations. */
	private boolean errorCommercialUse = false;

	/** Flag to return an error code for unreviewed files. */
	private boolean errorReviewStatus = false;

	/** Option method to return an error code for commercial use violations. */
	@AOption(shortName = 'c', longName = "error-commercial", description = "If present an error code is returned for commercial use violations.")
	public void enableCommercialUseError() {
		errorCommercialUse = true;
	}

	/** Option method to return an error code for unreviewed files. */
	@AOption(shortName = 'r', longName = "error-review", description = "If present an error code is returned for unreviewed files.")
	public void enableReviewStatusError() {
		errorReviewStatus = true;
	}

	/** {@inheritDoc} */
	@Override
	protected void doRun(PrintStream out) {
		boolean valid = true;

		// We use the strict (simple) boolean AND (&) to enforce the method gets
		// called even if a previous check returned false. Using the lazy AND
		// (&&) will omit execution of the method.
		valid &= checkLibraryDescriptorCoverage(out);
		valid &= checkLibraryDescriptorCorrectness(out);
		valid &= checkForUnusedLibraryDescriptors(out);
		valid &= checkLibraryDescriptorUniqueness(out);
		valid &= checkApacheCompatibility(out);

		boolean validCommercialUse = checkCommercialUse(out);
		if (errorCommercialUse) {
			valid &= validCommercialUse;
		}

		boolean validReviewStatus = checkLibraryDescriptorReviewStatus(out);
		if (errorReviewStatus) {
			valid &= validReviewStatus;
		}

		if (!valid) {
			System.exit(1);
		}
	}

	/** Validates the coverage of bundle libraries with library descriptors. */
	private boolean checkLibraryDescriptorCoverage(PrintStream out) {
		Set<File> uncoveredLibraries = getLibraryFiles();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			uncoveredLibraries.removeAll(descriptor.getLibraries());
		}

		Set<String> uncoveredLibraryNames = new HashSet<String>();
		for (File file : uncoveredLibraries) {
			uncoveredLibraryNames.add(file.getName());
		}

		return printErrorIfNotEmpty(out, uncoveredLibraryNames,
				"libraries without descriptor");
	}

	/** Validates whether a descriptor contains all necessary information. */
	private boolean checkLibraryDescriptorCorrectness(PrintStream out) {

		List<String> errors = new ArrayList<String>();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			addErrorIfEmpty(errors, descriptor, descriptor.getDescription(),
					"description");
			addErrorIfEmpty(errors, descriptor, descriptor.getWebsite(),
					"website");
			addErrorIfEmpty(errors, descriptor, descriptor.getVersion(),
					"version");

			addErrorIfEmpty(errors, descriptor, descriptor.getLicense()
					.getName(), "license");
			addErrorIfEmpty(errors, descriptor, descriptor.getLicense()
					.getWebsite(), "license.website");
		}

		return printErrorIfNotEmpty(out, errors,
				"malformed library descriptors");
	}

	/** Validates whether a descriptor actually covers one or more libraries. */
	private boolean checkForUnusedLibraryDescriptors(PrintStream out) {
		Set<LibraryDescriptor> emptyDescriptors = new HashSet<LibraryDescriptor>();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			if (descriptor.getLibraries().isEmpty()) {
				emptyDescriptors.add(descriptor);
			}
		}

		return printErrorIfNotEmpty(out, emptyDescriptors,
				"library descriptors without jar file");
	}

	/**
	 * Validates whether a jar file is covered by exactly one descriptor, i.e.
	 * there are two descriptors with one's name a prexif of another one.
	 */
	private boolean checkLibraryDescriptorUniqueness(PrintStream out) {
		Set<LibraryDescriptor> libraryDescriptors = getLibraryDescriptors();
		Set<LibraryDescriptor> prefixes = new HashSet<LibraryDescriptor>();
		for (LibraryDescriptor descriptor : libraryDescriptors) {
			for (LibraryDescriptor otherDescriptor : libraryDescriptors) {
				if (isPrefixOfAnother(descriptor, otherDescriptor)) {
					prefixes.add(otherDescriptor);
				}
			}
		}

		return printErrorIfNotEmpty(out, prefixes,
				"library descriptors are prefix of another descriptor in the same bundle");
	}

	/**
	 * Returns <code>true</code> if the library descriptor is prefix of another
	 * library descriptor in the same bundle.
	 */
	private boolean isPrefixOfAnother(LibraryDescriptor descriptor,
			LibraryDescriptor otherDescriptor) {

		if (descriptor.getId().equals(otherDescriptor.getId())) {
			return false;
		}

		// prefix conflicts are just interesting within the same bundle.
		if (!descriptor.getBundle().getId()
				.equals(otherDescriptor.getBundle().getId())) {
			return false;
		}

		return descriptor.getId().startsWith(otherDescriptor.getId());
	}

	/**
	 * Returns <code>true</code> if all library licenses are compatible with the
	 * Apache 2.0 license.
	 */
	private boolean checkApacheCompatibility(PrintStream out) {
		Set<LibraryDescriptor> incompatibleLibraries = new HashSet<LibraryDescriptor>();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			if (!descriptor.getLicense().isApacheCompatible()) {
				incompatibleLibraries.add(descriptor);
			}
		}

		return printErrorIfNotEmpty(out, incompatibleLibraries,
				"libraries are not compatible with the Apache 2.0 license");
	}

	/**
	 * Returns <code>true</code> if all library licenses allow commercial use of
	 * the library.
	 */
	private boolean checkCommercialUse(PrintStream out) {
		Set<LibraryDescriptor> incompatibleLibraries = new HashSet<LibraryDescriptor>();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			if (!descriptor.getLicense().isCommercialUseAllowed()) {
				incompatibleLibraries.add(descriptor);
			}
		}

		return printErrorIfNotEmpty(out, incompatibleLibraries,
				"libraries do not allow commercial use");
	}

	/** Return <code>true</code> if all library descriptors are reviewed. */
	private boolean checkLibraryDescriptorReviewStatus(PrintStream out) {
		Set<LibraryDescriptor> unreviewedDescriptors = new HashSet<LibraryDescriptor>();
		for (LibraryDescriptor descriptor : getLibraryDescriptors()) {
			try {
				String content = FileSystemUtils.readFile(descriptor
						.getDescriptorFile());
				ETrafficLightColor rating = RatingUtils
						.calculateRating(content);
				if (!ETrafficLightColor.GREEN.equals(rating)) {
					unreviewedDescriptors.add(descriptor);
				}
			} catch (IOException e) {
				CCSMAssert.fail("Library descriptor is not readable: "
						+ descriptor.getName());
			}
		}

		return printErrorIfNotEmpty(out, unreviewedDescriptors,
				"unreviewed library descriptors");
	}

	/**
	 * Prints an error if the list of descriptors is not empty.
	 * 
	 * @return <code>true</code> on if the descriptor list is empty, i.e. no
	 *         errors are present.
	 */
	private boolean printErrorIfNotEmpty(PrintStream out,
			Set<LibraryDescriptor> descriptors, String type) {
		Set<String> descriptorNames = new TreeSet<String>();
		for (LibraryDescriptor descriptor : descriptors) {
			descriptorNames.add(descriptor.getName() + " ("
					+ descriptor.getId() + ")");
		}
		return printErrorIfNotEmpty(out, descriptorNames, type);
	}

	/**
	 * Prints an error if the list of errors is not empty.
	 * 
	 * @return <code>true</code> on if the collection is empty, i.e. no errors
	 *         are present.
	 */
	private boolean printErrorIfNotEmpty(PrintStream out,
			Collection<String> errors, String type) {
		if (errors.size() == 0) {
			return true;
		}

		out.println(errors.size() + " " + type);
		for (String string : errors) {
			out.println(string);
		}
		out.println();

		return false;
	}

	/**
	 * Adds an error string to the list of errors if the provided string is
	 * empty.
	 */
	private void addErrorIfEmpty(List<String> errors,
			LibraryDescriptor descriptor, String value, String field) {
		if (StringUtils.isEmpty(value)) {
			errors.add(descriptor.getId() + ": Field " + field + " is missing.");
		}
	}
}
