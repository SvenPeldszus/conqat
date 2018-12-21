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
package org.conqat.engine.abap;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.conqat.engine.core.core.ConQATException;
import org.conqat.lib.commons.assertion.CCSMPre;
import org.conqat.lib.commons.filesystem.FileSystemUtils;

/**
 * Parser for path to an ABAP element.
 * 
 * @author $Author: pfaller $
 * @version $Rev: 51045 $
 * @ConQAT.Rating GREEN Hash: 64F3E9DE03A054530A01E9251EC790D7
 */
public class ParsedAbapElementPath {

	/**
	 * Pattern string to match paths of programs and main element of classes and
	 * interfaces
	 */
	private static final String PATTERN_PROGRAM_CLASS_INTERFACE = "(?<elementpath>(?<type>PROG|CLAS|INTF)/(?<name>[^/]+)\\.abap)";

	/** Pattern string to match paths of function group include elements */
	private static final String PATTERN_FUNCTION_GROUPS = "(?<elementpath>(?<type>FUGR)/(?<functiongroup>[^/]+)/(?<name>[^/]+)\\.abap)";

	/** Pattern string to match additional includes of classes and interfaces */
	private static final String PATTERN_CLASS_ADDITIONS = "(?<elementpath>(?<type>CLAS|INTF)/(?<classname>[^/]+)/(?<name>[^/]+)\\.abap)";

	/** Pattern string to match package path and pattern */
	private static final String PATTERN_PACKAGE_PATH = "(?<packagepath>.*?/?(?<packagename>[^/]+))/";

	/**
	 * Patterns for matching a file path which are used to identify ABAP element
	 * name and type.
	 */
	private static final Pattern[] ABAP_ELEMENT_PATH_PATTERNS = {
			Pattern.compile(".*" + PATTERN_PROGRAM_CLASS_INTERFACE),
			Pattern.compile(".*" + PATTERN_FUNCTION_GROUPS),
			Pattern.compile(".*" + PATTERN_CLASS_ADDITIONS) };
	/**
	 * Patterns for matching a file path which are used to identify the package
	 * path and package.
	 */
	private static final Pattern[] ABAP_PACKAGE_PATH_PATTERNS = {
			Pattern.compile(PATTERN_PACKAGE_PATH
					+ PATTERN_PROGRAM_CLASS_INTERFACE),
			Pattern.compile(PATTERN_PACKAGE_PATH + PATTERN_FUNCTION_GROUPS),
			Pattern.compile(PATTERN_PACKAGE_PATH + PATTERN_CLASS_ADDITIONS) };

	/** Flag to indicate if package path should be parsed, too. */
	private final boolean parsePackagePath;

	/** Parsed element name */
	private UniqueAbapElementName elementName;

	/** Parsed function group name */
	private String functionGroup;

	/** Parsed package name */

	private String packageName;

	/** The path of the element, without package path */
	private String elementPath;

	/** Parsed full package path */
	private String packagePath;

	/** Parsed full super package path */
	private String superPackagePath;

	/** Parsed class name for class additions. */
	private String className;

	/**
	 * Creates a new {@link ParsedAbapElementPath} and parses the given path
	 * including package path information.
	 * 
	 * @throws ConQATException
	 *             if path can not be parsed
	 */
	public ParsedAbapElementPath(String path) throws ConQATException {
		this(path, true);
	}

	/**
	 * Creates a new {@link ParsedAbapElementPath} and parses the given path.
	 * 
	 * @param parsePackagePath
	 *            if <code>true</code>, package information is parsed, too
	 * @throws ConQATException
	 *             if path can not be parsed
	 */
	public ParsedAbapElementPath(String path, boolean parsePackagePath)
			throws ConQATException {
		this.parsePackagePath = parsePackagePath;
		parsePath(path);
	}

	/**
	 * Performs parsing of the given path.
	 * 
	 * @throws ConQATException
	 *             if path can not be parsed
	 */
	private void parsePath(String path) throws ConQATException {
		path = FileSystemUtils.normalizeSeparators(path);
		Pattern[] patterns = ABAP_PACKAGE_PATH_PATTERNS;
		if (!parsePackagePath) {
			patterns = ABAP_ELEMENT_PATH_PATTERNS;
		}
		for (Pattern pattern : patterns) {
			if (checkMatchAndParse(path, pattern)) {
				return;
			}
		}
		throw new ConQATException("Path " + path
				+ " can not be parsed as path to an ABAP element (parsePath="
				+ parsePackagePath + ").");
	}

	/**
	 * Checks the if pattern matches the path. If so, path is parsed and
	 * attributes are set.
	 * 
	 * @return <code>true</code> if pattern did match, otherwise
	 *         <code>false</code>.
	 */
	private boolean checkMatchAndParse(String path, Pattern pattern) {
		Matcher matcher = pattern.matcher(path);
		if (matcher.matches()) {
			EAbapObjectType type = EAbapObjectType.valueOf(matcher
					.group("type"));
			String name = getGroupAndConvert(matcher, "name");
			this.elementName = new UniqueAbapElementName(name, type);
			this.elementPath = matcher.group("elementpath");
			if (type == EAbapObjectType.FUGR) {
				this.functionGroup = getGroupAndConvert(matcher,
						"functiongroup");
			}
			if (pattern.pattern().endsWith(PATTERN_CLASS_ADDITIONS)) {
				this.className = getGroupAndConvert(matcher, "classname");
			}
			if (parsePackagePath) {
				String unformattedPackageName = matcher.group("packagename");
				this.packageName = getGroupAndConvert(matcher, "packagename");
				this.packagePath = matcher.group("packagepath");
				this.superPackagePath = packagePath.substring(0,
						packagePath.length() - unformattedPackageName.length());
			}
			return true;
		}
		return false;
	}

	/**
	 * Gets the string of the given matching group form the matcher and converts
	 * the file name to ABAP name.
	 */
	private String getGroupAndConvert(Matcher matcher, String group) {
		return AbapCoreUtils.convertFileNameToAbapName(matcher.group(group));
	}

	/** @see #elementName */
	public UniqueAbapElementName getElementName() {
		return elementName;
	}

	/**
	 * Gets the path of an element without the package path.
	 * 
	 * The path is returned as file path, not in the ABAP representation. Thus
	 * '/' are path separators not ABAP namespace limiters.
	 */
	public String getElementPath() {
		return elementPath;
	}

	/**
	 * Gets the package name. Must not be called if package parsing was
	 * disabled.
	 * 
	 * The function package name is returned in the ABAP representation.
	 */
	public String getPackageName() {
		CCSMPre.isTrue(parsePackagePath,
				"Package name must not be accessed if package path parsing was disabled.");
		return packageName;
	}

	/**
	 * Gets the full package path. Must not be called if package parsing was
	 * disabled.
	 * 
	 * The path is returend as file path, not in the ABAP representation. Thus
	 * '/' are path separaters not ABAP namespace limiters.
	 */
	public String getPackagePath() {
		CCSMPre.isTrue(parsePackagePath,
				"Package path must not be accessed if package path parsing was disabled.");
		return packagePath;
	}

	/**
	 * Gets the super package path (without the package name). Must not be
	 * called if package parsing was disabled.
	 * 
	 * The path is returned as file path, not in the ABAP representation. Thus
	 * '/' are path separators not ABAP namespace limiters.
	 */
	public String getSuperPackagePath() {
		CCSMPre.isTrue(parsePackagePath,
				"Package path must not be accessed if package path parsing was disabled.");
		return superPackagePath;
	}

	/**
	 * Gets the function group name. Must be only called if path refered to a
	 * function group include element.
	 * 
	 * The function group name is returned in the ABAP representation.
	 */
	public String getFunctionGroup() {
		CCSMPre.isTrue(elementName.getObjectType() == EAbapObjectType.FUGR,
				"Function group name must be only accessed for function group elements.");
		return functionGroup;
	}

	/**
	 * Gets the class name (or interface name). Must be only called for class or
	 * interface elements. For class additions, this returns the containing
	 * class.
	 * 
	 * The class name is returned in the ABAP representation.
	 */
	public String getClassName() {
		CCSMPre.isTrue(elementName.getObjectType() == EAbapObjectType.CLAS
				|| elementName.getObjectType() == EAbapObjectType.INTF,
				"Class/interface name must be only called for class or interface elements.");

		if (className == null) {
			return elementName.getObjectName();
		}
		return className;
	}

}
