package jcl.pathnames;

import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.LogicalPathname;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * The {@code LogicalPathnameStruct} is the object representation of a Lisp 'logical-pathname' type.
 */
public class LogicalPathnameStruct extends PathnameStruct {

	private static final Pattern WORD_PATTERN = Pattern.compile("[A-Z0-9\\-]+");
	private static final Pattern WILDCARD_WORD_PATTERN = Pattern.compile("[A-Z0-9\\-\\*]+");

	private static final String HOST_MARKER = ":";
	private static final String DIRECTORY_MARKER = ";";
	private static final String TYPE_MARKER = ".";
	private static final String VERSION_MARKER = ".";

	private static final String WILDCARD_STRING = "*";
	private static final String BAD_WILDCARD_STRING = "**";

	private static final Pattern DIRECTORY_PATTERN = Pattern.compile(DIRECTORY_MARKER);

	private static final Logger LOGGER = LoggerFactory.getLogger(LogicalPathnameStruct.class);

	/**
	 * Public constructor.
	 *
	 * @param host      the logical-pathname host
	 * @param directory the logical-pathname directory
	 * @param name      the logical-pathname name
	 * @param type      the logical-pathname type
	 * @param version   the logical-pathname version
	 */
	private LogicalPathnameStruct(final PathnameHost host, final PathnameDirectory directory, final PathnameName name,
								  final PathnameType type, final PathnameVersion version) {
		super(LogicalPathname.INSTANCE, host, new PathnameDevice(PathnameComponentType.UNSPECIFIC), directory, name, type, version);
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname object elements
	 */
	public LogicalPathnameStruct(final String pathname) {
		this(getHost(pathname), getDirectory(pathname), getName(pathname), getType(pathname), getVersion(pathname));
	}

	/**
	 * This method gets the logical-pathname host.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname host
	 * @return the logical-pathname host
	 */
	private static PathnameHost getHost(final String pathname) {
		final String upperPathname = pathname.toUpperCase(Locale.getDefault());

		final int hostMarkerIndex = upperPathname.indexOf(HOST_MARKER);
		if (hostMarkerIndex == -1) {
			throw new TypeErrorException("Pathname must contain a host, even if it is empty: " + pathname);
		}

		final String pathnameHost = upperPathname.substring(0, hostMarkerIndex);

		if (WORD_PATTERN.matcher(pathnameHost).matches()) {
			return new PathnameHost(pathnameHost);
		}
		throw new TypeErrorException("Host did not match logical-pathname patterns: " + pathnameHost);
	}

	/**
	 * This method removed the host substring from the provided pathname string.
	 *
	 * @param pathname the pathname string to remove the host substring from
	 * @return the substring of the provided pathname string with the host removed
	 */
	private static String removeHost(final String pathname) {
		final int hostMarkerIndex = pathname.indexOf(HOST_MARKER);
		if (hostMarkerIndex == -1) {
			return pathname;
		}
		return pathname.substring(hostMarkerIndex + 1);
	}

	/**
	 * This method gets the logical-pathname directory.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname directory
	 * @return the logical-pathname directory
	 */
	private static PathnameDirectory getDirectory(final String pathname) {
		final String upperPathname = pathname.toUpperCase();

		String realPathname = removeHost(upperPathname);

		final PathnameDirectoryType directoryType;
		if (realPathname.startsWith(DIRECTORY_MARKER)) {
			directoryType = PathnameDirectoryType.RELATIVE;
			realPathname = realPathname.substring(1);
		} else {
			directoryType = PathnameDirectoryType.ABSOLUTE;
		}

		final int lastDirectoryMarkerIndex = realPathname.lastIndexOf(DIRECTORY_MARKER);
		if (lastDirectoryMarkerIndex == -1) {
			return new PathnameDirectory(PathnameComponentType.NIL);
		}

		final String directoryPathname = realPathname.substring(0, lastDirectoryMarkerIndex);
		final String[] directoryStrings = DIRECTORY_PATTERN.split(directoryPathname);

		final List<PathnameDirectoryLevel> directoryLevels = new ArrayList<>(directoryStrings.length);

		for (final String directoryString : directoryStrings) {
			if (directoryString.contains(BAD_WILDCARD_STRING)) {
				throw new TypeErrorException("** wildcard is not allowed in logical-pathname directory: " + directoryString);
			}

			final PathnameDirectoryLevel directoryLevel;
			if (WORD_PATTERN.matcher(directoryString).matches()) {
				directoryLevel = new PathnameDirectoryLevel(directoryString);
			} else if (WILDCARD_WORD_PATTERN.matcher(directoryString).matches()) {
				directoryLevel = new PathnameDirectoryLevel(directoryString);

				if (WILDCARD_STRING.equals(directoryString)) {
					directoryLevel.setDirectoryLevelType(PathnameDirectoryLevelType.WILD);
				}
			} else {
				throw new TypeErrorException("Directory did not match logical-pathname patterns: " + directoryString);
			}

			directoryLevels.add(directoryLevel);
		}

		final PathnameDirectoryComponent pathnameDirectoryComponent = new PathnameDirectoryComponent(directoryType, directoryLevels);
		return new PathnameDirectory(pathnameDirectoryComponent);
	}

	/**
	 * This method removed the directory substring from the provided pathname string.
	 *
	 * @param pathname the pathname string to remove the directory substring from
	 * @return the substring of the provided pathname string with the directory removed
	 */
	private static String removeDirectory(final String pathname) {
		final int lastDirectoryMarkerIndex = pathname.lastIndexOf(DIRECTORY_MARKER);
		if (lastDirectoryMarkerIndex == -1) {
			return pathname;
		}
		return pathname.substring(lastDirectoryMarkerIndex + 1);
	}

	/**
	 * This method gets the logical-pathname name.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname name
	 * @return the logical-pathname name
	 */
	private static PathnameName getName(final String pathname) {
		final String upperPathname = pathname.toUpperCase(Locale.getDefault());

		String realPathname = removeHost(upperPathname);
		realPathname = removeDirectory(realPathname);

		final String pathnameName;

		final int typeMarkerIndex = realPathname.indexOf(TYPE_MARKER);
		if (typeMarkerIndex == -1) {
			pathnameName = realPathname;
		} else {
			pathnameName = realPathname.substring(0, typeMarkerIndex);
		}

		if (pathnameName.contains(BAD_WILDCARD_STRING)) {
			throw new TypeErrorException("** wildcard is not allowed in logical-pathname name: " + pathnameName);
		}

		if (WORD_PATTERN.matcher(pathnameName).matches() || WILDCARD_WORD_PATTERN.matcher(pathnameName).matches()) {
			return new PathnameName(pathnameName);
		}
		throw new TypeErrorException("Name did not match logical-pathname patterns: " + pathnameName);
	}

	/**
	 * This method removed the name substring from the provided pathname string.
	 *
	 * @param pathname the pathname string to remove the name substring from
	 * @return the substring of the provided pathname string with the name removed
	 */
	private static String removeName(final String pathname) {
		final int typeMarkerIndex = pathname.indexOf(TYPE_MARKER);
		if (typeMarkerIndex == -1) {
			return pathname;
		}
		return pathname.substring(typeMarkerIndex + 1);
	}

	/**
	 * This method gets the logical-pathname type.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname type
	 * @return the logical-pathname type
	 */
	private static PathnameType getType(final String pathname) {
		final String upperPathname = pathname.toUpperCase();

		String realPathname = removeHost(upperPathname);
		realPathname = removeDirectory(realPathname);
		realPathname = removeName(realPathname);

		final String pathnameType;

		final int versionMarkerIndex = realPathname.indexOf(VERSION_MARKER);
		if (versionMarkerIndex == -1) {
			pathnameType = realPathname;
		} else {
			pathnameType = realPathname.substring(0, versionMarkerIndex);
		}

		if (pathnameType.contains(BAD_WILDCARD_STRING)) {
			throw new TypeErrorException("** wildcard is not allowed in logical-pathname type: " + pathnameType);
		}

		if (WORD_PATTERN.matcher(pathnameType).matches() || WILDCARD_WORD_PATTERN.matcher(pathnameType).matches()) {
			return new PathnameType(pathnameType);
		}
		throw new TypeErrorException("Type did not match logical-pathname patterns: " + pathnameType);
	}

	/**
	 * This method removed the type substring from the provided pathname string.
	 *
	 * @param pathname the pathname string to remove the type substring from
	 * @return the substring of the provided pathname string with the type removed
	 */
	private static String removeType(final String pathname) {
		final int versionMarkerIndex = pathname.indexOf(VERSION_MARKER);
		if (versionMarkerIndex == -1) {
			return pathname;
		}
		return pathname.substring(versionMarkerIndex + 1);
	}

	/**
	 * This method gets the logical-pathname version.
	 *
	 * @param pathname the pathname string to parse into the logical-pathname version
	 * @return the logical-pathname version
	 */
	private static PathnameVersion getVersion(final String pathname) {
		final String upperPathname = pathname.toUpperCase();

		String realPathname = removeHost(upperPathname);
		realPathname = removeDirectory(realPathname);
		realPathname = removeName(realPathname);
		final String pathnameVersion = removeType(realPathname);

		if (pathnameVersion.isEmpty()) {
			return new PathnameVersion();
		}

		if (":NEWEST".equalsIgnoreCase(pathnameVersion)) {
			return new PathnameVersion(PathnameVersionComponentType.NEWEST);
		}

		if ("*".equalsIgnoreCase(pathnameVersion)) {
			return new PathnameVersion(PathnameVersionComponentType.WILD);
		}

		try {
			final Integer versionNumber = Integer.parseInt(pathnameVersion);
			if (versionNumber > 0) {
				return new PathnameVersion(versionNumber);
			} else {
				throw new TypeErrorException("Version number must be a positive integer: " + pathnameVersion);
			}
		} catch (final NumberFormatException nfe) {
			LOGGER.trace("Provided version cannot be parsed as an integer: {}", pathnameVersion, nfe);
		}

		throw new TypeErrorException("Version did not match either '*', ':NEWEST', or a positive integer: " + pathnameVersion);
	}
}
