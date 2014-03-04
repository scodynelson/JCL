package jcl.structs.pathnames;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The {@code PathnameFileStruct} is the file-type object representation of a Lisp 'pathname' type.
 */
public class PathnameFileStruct extends PathnameStruct {

	private final PathnameDirectory directoryWithDirections;

	private static final Pattern PATHNAME_PATTERN = Pattern.compile(File.separator);
	private static final Pattern DRIVE_LETTER_PATTERN = Pattern.compile("([A-Z]|[a-z]):.");
	private static final String BACK_UP_STRING = "..";
	private static final String TILDE = "~";

	/**
	 * Public constructor.
	 *
	 * @param host      the pathname host
	 * @param device    the pathname device
	 * @param directory the pathname directory
	 * @param name      the pathname name
	 * @param type      the pathname type
	 * @param version   the pathname version
	 */
	public PathnameFileStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							  final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version);
		directoryWithDirections = getDirectoryWithDirections(directory);
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname the pathname string to parse into the pathname object elements
	 */
	public PathnameFileStruct(final String pathname) {
		this(getHost(), getDevice(pathname), getDirectory(pathname), getName(pathname), getType(pathname), getVersion());
	}

	/**
	 * Public constructor.
	 *
	 * @param path the path to parse into the pathname object elements
	 */
	public PathnameFileStruct(final Path path) {
		this(path.toFile().getPath());
	}

	/**
	 * Public constructor.
	 *
	 * @param file the file to parse into the pathname object elements
	 */
	public PathnameFileStruct(final File file) {
		this(file.getPath());
	}

	@Override
	public PathnameDirectory getPathnameDirectory() {
		return directoryWithDirections;
	}

	@Override
	public String toString() {
		return "PathnameFileStruct{"
				+ "host=" + host
				+ ", device=" + device
				+ ", directory=" + directory
				+ ", directoryWithDirections=" + directoryWithDirections
				+ ", name=" + name
				+ ", type=" + type
				+ ", version=" + version
				+ '}';
	}

	/**
	 * This method gets the pathname host.
	 *
	 * @return the pathname host
	 */
	private static PathnameHost getHost() {
		return new PathnameHost(null);
	}

	/**
	 * This method gets the pathname device.
	 *
	 * @param pathname the pathname string to parse into the pathname device
	 * @return the pathname device
	 */
	private static PathnameDevice getDevice(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		String pathPrefix = FilenameUtils.getPrefix(realPathname);

		// Strip off ':' or ':/' from drive letters
		if (StringUtils.endsWith(pathPrefix, ":") || StringUtils.endsWith(pathPrefix, ":/")) {
			pathPrefix = pathPrefix.substring(0, 1);
		}

		// If it is just the file path separator, no device here
		if (StringUtils.equals(pathPrefix, File.separator)) {
			pathPrefix = null;
		}

		return new PathnameDevice(pathPrefix);
	}

	/**
	 * This method gets the pathname directory.
	 *
	 * @param pathname the pathname string to parse into the pathname directory
	 * @return the pathname directory
	 */
	private static PathnameDirectory getDirectory(final String pathname) {
		final String realPathname = resolveUserHome(pathname);
		String directoryPath = FilenameUtils.getFullPathNoEndSeparator(realPathname);

		// Remove drive letter from the front if it exists
		if (DRIVE_LETTER_PATTERN.matcher(directoryPath).matches()) {
			directoryPath = StringUtils.substring(directoryPath, 2);
		} else if ((realPathname.length() == 2) && (realPathname.charAt(1) == ':')) {
			directoryPath = "";
		}

		final String[] tokens = PATHNAME_PATTERN.split(directoryPath);

		final List<String> directoryStrings = new ArrayList<>(tokens.length);
		directoryStrings.addAll(Arrays.asList(tokens));

		// This means that the path started with a '/'; thus, the resulting empty string artifact would need to be removed
		if (tokens[0].isEmpty()) {
			directoryStrings.remove("");
		}

		final Path path = Paths.get(realPathname);
		if (path.isAbsolute()) {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.ABSOLUTE);
		} else {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.RELATIVE);
		}
	}

	/**
	 * This method gets the pathname name.
	 *
	 * @param pathname the pathname string to parse into the pathname name
	 * @return the pathname name
	 */
	private static PathnameName getName(final String pathname) {
		final String realPathname = resolveUserHome(pathname);

		// This tests the case when the pathname is just a drive letter, in which case it should NOT be the name
		if ((realPathname.length() == 2) && (realPathname.charAt(1) == ':')) {
			return new PathnameName(null);
		}

		final String baseName = FilenameUtils.getBaseName(realPathname);
		return new PathnameName(baseName);
	}

	/**
	 * This method gets the pathname type.
	 *
	 * @param pathname the pathname string to parse into the pathname type
	 * @return the pathname type
	 */
	private static PathnameType getType(final String pathname) {
		final String realPathname = resolveUserHome(pathname);

		final int indexOfExtension = FilenameUtils.indexOfExtension(realPathname);
		if (indexOfExtension == -1) {
			return new PathnameType(null);
		}

		final String fileExtension = FilenameUtils.getExtension(realPathname);
		return new PathnameType(fileExtension);
	}

	/**
	 * This method gets the pathname version.
	 *
	 * @return the pathname version
	 */
	private static PathnameVersion getVersion() {
		return new PathnameVersion(null);
	}

	/**
	 * This method gets the pathname directory with '..' values replaced with :BACK or :UP, based on symbolic linking.
	 *
	 * @param pathnameDirectory the pathname directory
	 * @return the pathname directory with '..' values replaced with :BACK or :UP, based on symbolic linking
	 */
	private static PathnameDirectory getDirectoryWithDirections(final PathnameDirectory pathnameDirectory) {
		final List<String> directory = pathnameDirectory.getDirectory();
		final PathnameDirectoryType directoryType = pathnameDirectory.getPathnameDirectoryType();

		final List<String> directoryStringsWithDirections = new ArrayList<>(directory.size());
		for (final String token : directory) {
			// Leave ".." in the directory list and convert any :BACK encountered
			// to a ".." in directory list to maintain functionality with other functions
			if (BACK_UP_STRING.equals(token)) {
				final StringBuilder currentPathStringBuilder = new StringBuilder();
				if (directoryType == PathnameDirectoryType.ABSOLUTE) {
					currentPathStringBuilder.append(File.separatorChar);
				}

				final String currentPathString = StringUtils.join(directoryStringsWithDirections, File.separator);
				currentPathStringBuilder.append(currentPathString);

				final Path currentPath = Paths.get(currentPathStringBuilder.toString());

				// Back is for absolutes / up is for symbolic links
				if (Files.isSymbolicLink(currentPath)) {
					directoryStringsWithDirections.add(PathnameDirectoryDirectionType.UP.getValue());
				} else {
					directoryStringsWithDirections.add(PathnameDirectoryDirectionType.BACK.getValue());
				}
			} else {
				directoryStringsWithDirections.add(token);
			}
		}
		return new PathnameDirectory(directoryStringsWithDirections, directoryType);
	}

	/**
	 * This method resolves special UserHome system properties when '~' values are encountered and should be parsed as such.
	 *
	 * @param pathname the pathname string to resolve special UserHome system properties
	 * @return a new pathname string with resolved special UserHome system properties
	 */
	private static String resolveUserHome(final String pathname) {
		//there are special situations involving tildes in pathname
		//handle a tilde at start of pathname; expand it to the user's directory

		final String userHome = System.getProperty("user.home");

		String realPathname = pathname;
		if (TILDE.equals(pathname)) {
			realPathname = userHome;
		} else if (pathname.startsWith("~/") || pathname.startsWith("~\\")) {
			realPathname = pathname.replace(TILDE, userHome);
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
}
