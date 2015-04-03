/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link PathnameFileStruct} is the file-type object representation of a Lisp 'pathname' type.
 */
public class PathnameFileStruct extends PathnameStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6116704485090857692L;

	/**
	 * Back/Up string for pathname parsing.
	 */
	private static final String BACK_UP_STRING = "..";

	/**
	 * Wildcard string for pathname parsing.
	 */
	private static final String WILDCARD_STRING = "*";

	/**
	 * Tilde string for Unix home directories.
	 */
	private static final String TILDE = "~";

	/**
	 * {@link Pattern} used to parse pathname directories.
	 * <p>
	 * NOTE: This patterns complexity is due to Windows platforms and their usage of backslashes
	 */
	private static final Pattern PATHNAME_PATTERN = Pattern.compile((File.separatorChar == '\\') ? "\\\\" : File.separator);

	/**
	 * {@link Pattern} used to parse Drive letters for Windows platforms.
	 */
	private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("([A-Z]|[a-z]):.");

	/**
	 * {@link Pattern} used to parse '~' strings combinations.
	 */
	private static final Pattern TILDE_PATTERN = Pattern.compile(TILDE, Pattern.LITERAL);

	/**
	 * Int constant to note the length of a Drive letter (with backslash) for Windows platforms.
	 */
	private static final int DRIVE_LETTER_LENGTH = 2;

	/**
	 * Public constructor.
	 *
	 * @param path
	 * 		the path to parse into the pathname object elements
	 */
	public PathnameFileStruct(final Path path) {
		this(path.toFile());
	}

	/**
	 * Public constructor.
	 *
	 * @param file
	 * 		the file to parse into the pathname object elements
	 */
	public PathnameFileStruct(final File file) {
		this(file.getPath());
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 */
	public PathnameFileStruct(final String pathname) {
		super(getHost(), getDevice(pathname), getDirectory(pathname), getName(pathname), getType(pathname), getVersion(), Paths.get(pathname));
	}

	/**
	 * Public constructor.
	 *
	 * @param host
	 * 		the pathname host
	 * @param device
	 * 		the pathname device
	 * @param directory
	 * 		the pathname directory
	 * @param name
	 * 		the pathname name
	 * @param type
	 * 		the pathname type
	 * @param version
	 * 		the pathname version
	 */
	public PathnameFileStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                          final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version, getPathFromComponents(host, device, directory, name, type, version));
	}

	/**
	 * Gets the pathname host.
	 *
	 * @return the pathname host
	 */
	private static PathnameHost getHost() {
		return new PathnameHost(PathnameComponentType.UNSPECIFIC);
	}

	/**
	 * Gets the pathname device.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname device
	 *
	 * @return the pathname device
	 */
	private static PathnameDevice getDevice(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		String pathPrefix = FilenameUtils.getPrefix(realPathname);

		// Strip off ':' or ':/' from drive letters
		if (StringUtils.endsWith(pathPrefix, ":") || StringUtils.endsWith(pathPrefix, ":/")) {
			pathPrefix = pathPrefix.substring(0, 1);
		}

		// If it is just the file path separator or it is empty, no device here
		if (StringUtils.equals(pathPrefix, File.separator)) {
			return null;
		} else if (StringUtils.isEmpty(pathPrefix)) {
			return new PathnameDevice(PathnameComponentType.NIL);
		} else {
			return new PathnameDevice(pathPrefix);
		}
	}

	/**
	 * Resolves special UserHome system properties when '~' values are encountered and should be parsed as such.
	 *
	 * @param pathname
	 * 		the pathname string to resolve special UserHome system properties
	 *
	 * @return a new pathname string with resolved special UserHome system properties
	 */
	private static String resolveUserHome(final String pathname) {
		//there are special situations involving tildes in pathname
		//handle a tilde at start of pathname; expand it to the user's directory

		final String userHome = SystemUtils.USER_HOME;

		String realPathname = pathname;
		if (TILDE.equals(pathname)) {
			realPathname = userHome;
		} else if (pathname.startsWith("~/") || pathname.startsWith("~\\")) {
			realPathname = TILDE_PATTERN.matcher(pathname).replaceAll(userHome);
		} else if (pathname.startsWith(TILDE)) {
			final int lastPathSeparatorIndex = userHome.lastIndexOf(File.separatorChar);
			final String usersDir = userHome.substring(0, lastPathSeparatorIndex);

			final boolean containsSeparator = pathname.contains(File.separator);
			if (containsSeparator) {
				final String username = pathname.substring(1, pathname.indexOf(File.separatorChar));
				final String restOfPath = pathname.substring(pathname.indexOf(File.separatorChar));
				realPathname = usersDir + File.separatorChar + username + restOfPath;
			} else {
				final String username = pathname.substring(1);
				realPathname = usersDir + File.separatorChar + username;
			}
		}
		return FilenameUtils.separatorsToSystem(realPathname);
	}

	/**
	 * Gets the pathname directory.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname directory
	 *
	 * @return the pathname directory
	 */
	private static PathnameDirectory getDirectory(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		String directoryPath = FilenameUtils.getFullPathNoEndSeparator(realPathname);

		// This is used for building the path piece by piece so that we can detect symbolic links
		final StringBuilder currentPathBuilder = new StringBuilder();

		// Remove drive letter from the front if it exists
		if (DRIVE_LETTER_PATTERN.matcher(directoryPath).matches()) {
			directoryPath = StringUtils.substring(directoryPath, DRIVE_LETTER_LENGTH);

			final String pathPrefix = FilenameUtils.getPrefix(realPathname);
			currentPathBuilder.append(pathPrefix);
		} else if ((realPathname.length() == DRIVE_LETTER_LENGTH) && (realPathname.charAt(1) == ':')) {
			directoryPath = "";
		}
		currentPathBuilder.append(File.separatorChar);

		final String[] tokens = PATHNAME_PATTERN.split(directoryPath);

		final List<String> directoryStrings = new ArrayList<>(tokens.length);
		directoryStrings.addAll(Arrays.asList(tokens));

		// This means that the path started with a '/'; thus, the resulting empty string artifact would need to be removed
		if (tokens[0].isEmpty()) {
			directoryStrings.remove("");
		}

		if (directoryStrings.isEmpty()) {
			// No directories. Go ahead and return.
			return null;
		}

		final Path path = Paths.get(realPathname);
		final boolean isAbsolute = path.isAbsolute();
		final PathnameDirectoryType directoryType = isAbsolute ? PathnameDirectoryType.ABSOLUTE : PathnameDirectoryType.RELATIVE;

		final List<PathnameDirectoryLevel> directoryLevels = new ArrayList<>(directoryStrings.size());

		for (final String directoryString : directoryStrings) {

			currentPathBuilder.append(File.separatorChar);
			currentPathBuilder.append(directoryString);

			PathnameDirectoryLevelType directoryLevelType = PathnameDirectoryLevelType.NULL;

			// Leave ".." in the directory list and convert any :BACK encountered
			// to a ".." in directory list to maintain functionality with other functions
			if (BACK_UP_STRING.equals(directoryString)) {
				final Path currentPath = Paths.get(currentPathBuilder.toString());

				// Back is for absolutes / up is for symbolic links
				if (Files.isSymbolicLink(currentPath)) {
					directoryLevelType = PathnameDirectoryLevelType.UP;
				} else {
					directoryLevelType = PathnameDirectoryLevelType.BACK;
				}
			} else if (WILDCARD_STRING.equals(directoryString)) {
				directoryLevelType = PathnameDirectoryLevelType.WILD;
			}

			final PathnameDirectoryLevel directoryLevel = new PathnameDirectoryLevel(directoryString, directoryLevelType);
			directoryLevels.add(directoryLevel);
		}

		final PathnameDirectoryComponent pathnameDirectoryComponent = new PathnameDirectoryComponent(directoryType, directoryLevels);
		return new PathnameDirectory(pathnameDirectoryComponent);
	}

	/**
	 * Gets the pathname name.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname name
	 *
	 * @return the pathname name
	 */
	private static PathnameName getName(final String pathname) {
		final String realPathname = resolveUserHome(pathname);

		// This tests the case when the pathname is just a drive letter, in which case it should NOT be the name
		if ((realPathname.length() == DRIVE_LETTER_LENGTH) && (realPathname.charAt(1) == ':')) {
			return null;
		}

		final String baseName = FilenameUtils.getBaseName(realPathname);
		if (StringUtils.isEmpty(baseName)) {
			return null;
		} else {
			return new PathnameName(baseName);
		}
	}

	/**
	 * Gets the pathname type.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname type
	 *
	 * @return the pathname type
	 */
	private static PathnameType getType(final String pathname) {
		final String realPathname = resolveUserHome(pathname);

		final int indexOfExtension = FilenameUtils.indexOfExtension(realPathname);
		if (indexOfExtension == -1) {
			return null;
		}

		final String fileExtension = FilenameUtils.getExtension(realPathname);
		if (StringUtils.isEmpty(fileExtension)) {
			return null;
		} else {
			return new PathnameType(fileExtension);
		}
	}

	/**
	 * Gets the pathname version.
	 *
	 * @return the pathname version
	 */
	private static PathnameVersion getVersion() {
		return new PathnameVersion(PathnameVersionComponentType.NEWEST);
	}
}
