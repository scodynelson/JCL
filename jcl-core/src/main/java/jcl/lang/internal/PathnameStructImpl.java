/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.pathname.PathnameComponentType;
import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameDirectoryComponent;
import jcl.lang.pathname.PathnameDirectoryLevel;
import jcl.lang.pathname.PathnameDirectoryLevelType;
import jcl.lang.pathname.PathnameDirectoryType;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.pathname.PathnameVersionComponentType;
import jcl.lang.statics.PrinterVariables;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link PathnameStructImpl} is the object representation of a Lisp 'pathname' type.
 */
public class PathnameStructImpl extends BuiltInClassStruct implements PathnameStruct {

	private static final String CURRENT_DIR_STRING = ".";

	private static final String CURRENT_DIR_STRING_SLASH = "./";

	private static final String CURRENT_DIR_STRING_BACKSLASH = ".\\";

	/**
	 * Back/Up string for pathname parsing.
	 */
	private static final String BACK_UP_STRING = "..";

	/**
	 * Back/Up string for pathname parsing.
	 */
	private static final String BACK_UP_STRING_SLASH = "../";

	/**
	 * Wildcard string for pathname parsing.
	 */
	private static final String WILDCARD_STRING = "*";

	/**
	 * Wildcard-Inferiors string for pathname parsing.
	 */
	private static final String WILDCARD_INFERIORS_STRING = "**";

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
	 * The {@link PathnameHost} value.
	 */
	protected PathnameHost host = new PathnameHost(PathnameComponentType.UNSPECIFIC);

	/**
	 * The {@link PathnameDevice} value.
	 */
	protected PathnameDevice device = new PathnameDevice(PathnameComponentType.NIL);

	/**
	 * The {@link PathnameDirectory} value.
	 */
	protected PathnameDirectory directory = new PathnameDirectory(PathnameComponentType.NIL);

	/**
	 * The {@link PathnameName} value.
	 */
	protected PathnameName name = new PathnameName(PathnameComponentType.NIL);

	/**
	 * The {@link PathnameType} value.
	 */
	protected PathnameType type = new PathnameType(PathnameComponentType.NIL);

	/**
	 * The {@link PathnameVersion} value.
	 */
	protected PathnameVersion version = new PathnameVersion(PathnameVersionComponentType.NIL);

	/**
	 * The internal {@link URI} representation of the pathname.
	 */
	protected final URI uri;

	private String namestring;

	/**
	 * Public constructor.
	 *
	 * @param path
	 * 		the path to parse into the pathname object elements
	 */
	public PathnameStructImpl(final Path path) {
		// TODO: This doesn't work correctly!!!
		this(path.toUri());
	}

	/**
	 * Public constructor.
	 *
	 * @param file
	 * 		the file to parse into the pathname object elements
	 */
	public PathnameStructImpl(final File file) {
		// TODO: This doesn't work correctly!!!
		this(file.toURI());
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 */
	public PathnameStructImpl(final String pathname) {
		this(getURIFromPathname(pathname));
//		init(pathnameString);
	}

	/**
	 * Public constructor.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname object elements
	 */
	public PathnameStructImpl(final URI uri) {
		this(jcl.type.PathnameType.INSTANCE, getHost(uri), getDevice(uri), getDirectory(uri), getName(uri), getType(uri), getVersion(), uri);
	}

	/**
	 * Protected constructor.
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
	public PathnameStructImpl(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                           final PathnameName name, final PathnameType type, final PathnameVersion version) {
		this(jcl.type.PathnameType.INSTANCE, host, device, directory, name, type, version, getURIFromComponents(host, device, directory, name, type, version));
	}

	/**
	 * Protected constructor.
	 *
	 * @param pathnameType
	 * 		the pathname structure type
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
	 * @param uri
	 * 		the {@link URI} to parse into the pathname object elements
	 */
	public PathnameStructImpl(final jcl.type.PathnameType pathnameType,
	                             final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                             final PathnameName name, final PathnameType type, final PathnameVersion version,
	                             final URI uri) {
		super(pathnameType, null, null);
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
		this.uri = uri;
	}

	@Override
	public Path getPath() {
		final String namestring = getNamestring();
		final File file = new File(namestring);
		return file.toPath();
	}

	@Override
	public boolean exists() {
		final String namestring = getNamestring();
		final File file = new File(namestring);
		return file.exists();
	}

	/**
	 * Gets the pathname host.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname host
	 *
	 * @return the pathname host
	 */
	private static PathnameHost getHost(final URI uri) {
		if (uri.isOpaque()) {
			final String schemeSpecificPart = uri.getSchemeSpecificPart();
			return new PathnameHost(schemeSpecificPart);
		}

		final String authority = uri.getAuthority();
		if (authority == null) {
			return new PathnameHost(PathnameComponentType.UNSPECIFIC);
		}
		return new PathnameHost(authority);
	}

	/**
	 * Gets the pathname device.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname device
	 *
	 * @return the pathname device
	 */
	private static PathnameDevice getDevice(final URI uri) {
		final String scheme = uri.getScheme();
		return new PathnameDevice(scheme);
	}

	/**
	 * Gets the pathname directory.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname directory
	 *
	 * @return the pathname directory
	 */
	private static PathnameDirectory getDirectory(final URI uri) {
		final String uriPath = StringUtils.defaultString(uri.getPath());
		final String realURIPath = resolveUserHome(uriPath);

		String directoryPath = FilenameUtils.getFullPathNoEndSeparator(realURIPath);

		// This is used for building the path piece by piece so that we can detect symbolic links
		final StringBuilder currentPathBuilder = new StringBuilder();

		// Remove drive letter from the front if it exists
		if (DRIVE_LETTER_PATTERN.matcher(directoryPath).matches()) {
			directoryPath = StringUtils.substring(directoryPath, DRIVE_LETTER_LENGTH);

			final String pathPrefix = FilenameUtils.getPrefix(realURIPath);
			currentPathBuilder.append(pathPrefix);
		} else if ((realURIPath.length() == DRIVE_LETTER_LENGTH) && (realURIPath.charAt(1) == ':')) {
			directoryPath = "";
		}
		currentPathBuilder.append(File.separatorChar);

		final String[] tokens = PATHNAME_PATTERN.split(directoryPath);
		if (tokens.length == 0) {
			// No directories. Go ahead and return.
			return null;
		}

		final List<String> directoryStrings = new ArrayList<>(tokens.length);
		directoryStrings.addAll(Arrays.asList(tokens));

		// This means that the path started with a '/'; thus, the resulting empty string artifact would need to be removed
		if (tokens[0].isEmpty()) {
			directoryStrings.remove(0);

			if (directoryStrings.isEmpty()) {
				// Re-check for no directories. Go ahead and return.
				return null;
			}
		}

		final boolean isURIAbsolute = uri.isAbsolute();

		final File uriAsFile = new File(uri.toString());
		final boolean isFileAbsolute = uriAsFile.isAbsolute();

		final PathnameDirectoryType directoryType = (isURIAbsolute || isFileAbsolute) ? PathnameDirectoryType.ABSOLUTE : PathnameDirectoryType.RELATIVE;

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
	 * Gets the pathname name.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname name
	 *
	 * @return the pathname name
	 */
	private static PathnameName getName(final URI uri) {
		final String urlPath = uri.getPath();
		if (StringUtils.isEmpty(urlPath)) {
			return null;
		}

		final String baseName = FilenameUtils.getBaseName(urlPath);
		return new PathnameName(baseName);
	}

	/**
	 * Gets the pathname type.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname type
	 *
	 * @return the pathname type
	 */
	private static PathnameType getType(final URI uri) {
		final String urlPath = uri.getPath();
		if (StringUtils.isEmpty(urlPath)) {
			return null;
		}

		final String fileExtension = FilenameUtils.getExtension(urlPath);

		final StringBuilder typeStringBuilder = new StringBuilder(fileExtension);

		// add query, if any, to type element
		final String query = uri.getQuery();
		if (query != null) {
			typeStringBuilder.append('?');
			typeStringBuilder.append(query);
		}

		// add fragment, if any, to type element
		final String fragment = uri.getFragment();
		if (fragment != null) {
			typeStringBuilder.append('#');
			typeStringBuilder.append(fragment);
		}

		return new PathnameType(typeStringBuilder.toString());
	}

	/**
	 * Gets the pathname version.
	 *
	 * @return the pathname version
	 */
	private static PathnameVersion getVersion() {
		return null;
	}

	/**
	 * Gets a {@link URI} from the provided {@code pathname}.
	 *
	 * @param pathname
	 * 		the pathname string to convert to a {@link URI}
	 *
	 * @return the {@link URI} value of the provided {@code pathname}
	 */
	private static URI getURIFromPathname(final String pathname) {
		return URI.create(pathname);
	}

	@Override
	public PathnameHost getPathnameHost() {
		return host;
	}

	@Override
	public PathnameDevice getPathnameDevice() {
		return device;
	}

	@Override
	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	@Override
	public PathnameName getPathnameName() {
		return name;
	}

	@Override
	public PathnameType getPathnameType() {
		return type;
	}

	@Override
	public PathnameVersion getPathnameVersion() {
		return version;
	}

	@Override
	public URI getUri() {
		return uri;
	}

	/**
	 * Gets a pathname namestring from the provided pathname components.
	 *
	 * @param pathnameHost
	 * 		the pathname host
	 * @param pathnameDevice
	 * 		the pathname device
	 * @param pathnameDirectory
	 * 		the pathname directory
	 * @param pathnameName
	 * 		the pathname name
	 * @param pathnameType
	 * 		the pathname type
	 * @param pathnameVersion
	 * 		the pathname version
	 *
	 * @return the pathname namestring constructed from the provided pathname components
	 */
	protected static URI getURIFromComponents(final PathnameHost pathnameHost, final PathnameDevice pathnameDevice,
	                                          final PathnameDirectory pathnameDirectory, final PathnameName pathnameName,
	                                          final PathnameType pathnameType, final PathnameVersion pathnameVersion) {

		final StringBuilder stringBuilder = new StringBuilder();

		if (pathnameDevice != null) {
			final String device = pathnameDevice.getDevice();
			if (device != null) {
				stringBuilder.append(device);
				stringBuilder.append("://");
			}
		}

		if (pathnameHost != null) {
			final String host = pathnameHost.getHost();
			if (host != null) {
				stringBuilder.append(host);
			}
		}

		if (pathnameDirectory != null) {
			final PathnameDirectoryComponent directoryComponent = pathnameDirectory.getDirectoryComponent();
			if (directoryComponent != null) {

				final PathnameDirectoryType directoryType = directoryComponent.getPathnameDirectoryType();
				switch (directoryType) {
					case ABSOLUTE:
						stringBuilder.append('/');
						break;
					case RELATIVE:
						break;
				}

				final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();
				if (directoryLevels != null) {
					for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {

						final PathnameDirectoryLevelType levelType = directoryLevel.getDirectoryLevelType();
						switch (levelType) {
							case WILD:
								stringBuilder.append('*');
								break;
							case BACK:
								stringBuilder.append("..");
								break;
							case UP:
								stringBuilder.append("..");
								break;
							case NULL:
								final String level = directoryLevel.getDirectoryLevel();
								stringBuilder.append(level);
								break;
						}
						stringBuilder.append('/');
					}
				}
			} else {
				final PathnameComponentType componentType = pathnameDirectory.getComponentType();
				switch (componentType) {
					case UNSPECIFIC:
						break;
					case WILD:
						stringBuilder.append('*');
						break;
					case NIL:
						break;
				}
			}
		}

		if (pathnameName != null) {
			final String name = pathnameName.getName();
			if (name != null) {
				stringBuilder.append(name);
			}
		}

		if (pathnameType != null) {
			final String type = pathnameType.getType();
			if (type != null) {
				stringBuilder.append('.');
				stringBuilder.append(type);
			}
		}

		if (pathnameVersion != null) {
			final Integer version = pathnameVersion.getVersion();
			if (version != null) {
				stringBuilder.append('.');
				stringBuilder.append(version);
			}
		}

		return getURIFromPathname(stringBuilder.toString());
	}

	@Override
	public String getNamestring() {
		return uri.toString();
	}

	private void init(final String pathnameString) {
		if (pathnameString == null) {
			return;
		}

		if (CURRENT_DIR_STRING.equals(pathnameString) || CURRENT_DIR_STRING_SLASH.equals(pathnameString)
				|| (SystemUtils.IS_OS_WINDOWS && CURRENT_DIR_STRING_BACKSLASH.equals(pathnameString))) {
			final PathnameDirectoryComponent directoryComponent
					= new PathnameDirectoryComponent(PathnameDirectoryType.RELATIVE);
			directory = new PathnameDirectory(directoryComponent);
			return;
		}
		if (BACK_UP_STRING.equals(pathnameString) || BACK_UP_STRING_SLASH.equals(pathnameString)) {
			final PathnameDirectoryLevel directoryLevel
					= new PathnameDirectoryLevel(PathnameDirectoryLevelType.UP);
			final PathnameDirectoryComponent directoryComponent
					= new PathnameDirectoryComponent(PathnameDirectoryType.RELATIVE, Collections.singletonList(directoryLevel));
			directory = new PathnameDirectory(directoryComponent);
			return;
		}

		if (SystemUtils.IS_OS_WINDOWS && (pathnameString.startsWith("\\\\") || pathnameString.startsWith("//"))) {
			// UNC path support
			final int shareIndex;
			final int dirIndex;
			// match \\<server>\<share>\[directories-and-files]
			if (pathnameString.startsWith("\\\\")) {
				shareIndex = pathnameString.indexOf('\\', 2);
				dirIndex = pathnameString.indexOf('\\', shareIndex + 1);
				// match //<server>/<share>/[directories-and-files]
			} else {
				shareIndex = pathnameString.indexOf('/', 2);
				dirIndex = pathnameString.indexOf('/', shareIndex + 1);
			}
			if ((shareIndex == -1) || (dirIndex == -1)) {
				throw new ErrorException("Unsupported UNC path format: \"" + pathnameString + '"');
			}

			host = new PathnameHost(pathnameString.substring(2, shareIndex));
			device = new PathnameDevice(pathnameString.substring(shareIndex + 1, dirIndex));

			final PathnameStructImpl p = new PathnameStructImpl(pathnameString.substring(dirIndex));
			directory = p.directory;
			name = p.name;
			type = p.type;
			version = p.version;
			return;
		}

		String s1 = FilenameUtils.separatorsToUnix(pathnameString);

		// Expand user home directories
		if (SystemUtils.IS_OS_UNIX) {
			final String userHome = SystemUtils.USER_HOME;
			if ("~".equals(s1)) {
				s1 = userHome + '/';
			} else if (s1.startsWith("~/")) {
				s1 = userHome + s1.substring(1);
			}
		}

		namestring = s1;

		String currentPathnameString = s1;
		if (SystemUtils.IS_OS_WINDOWS) {
			// Device on Windows is the Drive Letter if it is part of the pathname
			if ((currentPathnameString.length() >= 2) && (currentPathnameString.charAt(1) == ':')) {
				device = new PathnameDevice(String.valueOf(currentPathnameString.charAt(0)));
				currentPathnameString = currentPathnameString.substring(2);
			}
		}

		String directoryString = null;

		final int currPathLength = currentPathnameString.length();
		if (currentPathnameString.charAt(currPathLength) == File.separatorChar) {
			directoryString = currentPathnameString.substring(0, currPathLength + 1);
			currentPathnameString = currentPathnameString.substring(currPathLength + 1);

			if ("..".equals(currentPathnameString)) {
				directoryString += currentPathnameString;
				currentPathnameString = "";
			}
		}

		if (directoryString != null) {
			directory = parseDirectory(directoryString);
		}

		if (currentPathnameString.startsWith(".") && ((currentPathnameString.indexOf('.', 1) == -1) || currentPathnameString.endsWith("."))) {
			name = new PathnameName(currentPathnameString);
			return;
		}

		final int index = currentPathnameString.lastIndexOf('.');
		String pathnameName = null;
		String pathnameType = null;
		if (index > 0) {
			pathnameName = currentPathnameString.substring(0, index);
			pathnameType = currentPathnameString.substring(index + 1);
		} else if (!currentPathnameString.isEmpty()) {
			pathnameName = currentPathnameString;
		}

		if ("*".equals(pathnameName)) {
			name = new PathnameName(PathnameComponentType.WILD);
		} else {
			name = new PathnameName(pathnameName);
		}

		if ("*".equals(pathnameType)) {
			type = new PathnameType(PathnameComponentType.WILD);
		} else {
			type = new PathnameType(pathnameType);
		}
	}

	private static PathnameDirectory parseDirectory(final String directoryString) {
		if ("/".equals(directoryString) || (SystemUtils.IS_OS_WINDOWS && "\\".equals(directoryString))) {
			final PathnameDirectoryComponent directoryComponent = new PathnameDirectoryComponent(PathnameDirectoryType.ABSOLUTE);
			return new PathnameDirectory(directoryComponent);
		}

		// This is used for building the path piece by piece so that we can detect symbolic links
		final StringBuilder currentPathBuilder = new StringBuilder();

		final PathnameDirectoryType directoryType;
		if (directoryString.startsWith(File.separator)) {
			directoryType = PathnameDirectoryType.ABSOLUTE;
			currentPathBuilder.append(File.separator);
		} else {
			directoryType = PathnameDirectoryType.RELATIVE;
		}

		final List<PathnameDirectoryLevel> directoryLevels = new ArrayList<>();

		final StringTokenizer st = new StringTokenizer(directoryString, File.separator);
		while (st.hasMoreTokens()) {
			final String token = st.nextToken();

			currentPathBuilder.append(token);

			PathnameDirectoryLevelType directoryLevelType = PathnameDirectoryLevelType.NULL;
			String directoryLevelString = null;
			if ("*".equals(token)) {
				directoryLevelType = PathnameDirectoryLevelType.WILD;
			} else if ("**".equals(token)) {
				directoryLevelType = PathnameDirectoryLevelType.WILD_INFERIORS;
			} else if ("..".equals(token)) {
				final Path currentPath = Paths.get(currentPathBuilder.toString());

				// Back is for absolutes / up is for symbolic links
				if (Files.isSymbolicLink(currentPath)) {
					directoryLevelType = PathnameDirectoryLevelType.UP;
				} else {
					directoryLevelType = PathnameDirectoryLevelType.BACK;
				}
			} else {
				directoryLevelString = token;
			}

			currentPathBuilder.append(File.separatorChar);

			final PathnameDirectoryLevel directoryLevel = new PathnameDirectoryLevel(directoryLevelString, directoryLevelType);
			directoryLevels.add(directoryLevel);
		}

		final PathnameDirectoryComponent pathnameDirectoryComponent = new PathnameDirectoryComponent(directoryType, directoryLevels);
		return new PathnameDirectory(pathnameDirectoryComponent);
	}

	public String getNamestringNew() {
		if (namestring != null) {
			return namestring;
		}
		if ((name.getComponentType() == PathnameComponentType.NIL) && (type.getComponentType() != PathnameComponentType.NIL)) {
			return null;
		}

		final StringBuilder sb = new StringBuilder();
		final String hostString = host.getHost();
		if (hostString != null) {
			if (this instanceof LogicalPathnameStruct) {
				sb.append(hostString);
				sb.append(':');
			} else {
				// A UNC path
				sb.append("//").append(hostString).append('/');
			}
		}

		final String deviceString = device.getDevice();
		if (deviceString != null) {
			sb.append(deviceString);
			if ((this instanceof LogicalPathnameStruct) || (hostString == null)) {
				sb.append(':'); // non-UNC paths
			}
		} else {
			throw new ErrorException("Device cannot be null.");
		}

		final String directoryNamestring = getDirectoryNamestring();
		sb.append(directoryNamestring);
		final String nameValue = name.getName();
		if (nameValue != null) {
			if (nameValue.indexOf('/') >= 0) {
				if (namestring == null) {
					throw new ErrorException("Namestring null???.");
				}
				return null;
			}
			sb.append(nameValue);
		} else if (name.getComponentType() == PathnameComponentType.WILD) {
			sb.append(WILDCARD_STRING);
		}

		final String typeValue = type.getType();
		if (typeValue != null) {
			sb.append('.');
			sb.append(typeValue);
		} else if (type.getComponentType() == PathnameComponentType.WILD) {
			sb.append('.');
			sb.append('*');
		}

		if (this instanceof LogicalPathnameStruct) {
			final Integer versionValue = version.getVersion();
			if (versionValue != null) {
				sb.append('.');
				sb.append(versionValue);
			} else if (version.getComponentType() == PathnameVersionComponentType.WILD) {
				sb.append('.');
				sb.append(WILDCARD_STRING);
			} else if (version.getComponentType() == PathnameVersionComponentType.NEWEST) {
				sb.append('.');
				sb.append("NEWEST");
			}
		}
		namestring = sb.toString();
		return namestring;
	}

	private boolean validateDirectory() {
		final PathnameDirectoryComponent directoryComponent = directory.getDirectoryComponent();
		final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();

		if (directoryLevels.isEmpty()) {
			return true;
		}

		final Iterator<PathnameDirectoryLevel> iterator = directoryLevels.iterator();
		PathnameDirectoryLevel directoryLevel = iterator.next();
		while (iterator.hasNext()) {
			final PathnameDirectoryLevelType directoryLevelType = directoryLevel.getDirectoryLevelType();
			if (directoryLevelType == PathnameDirectoryLevelType.WILD_INFERIORS) {
				final PathnameDirectoryLevel next = iterator.next();
				final PathnameDirectoryLevelType nextDirectoryLevelType = next.getDirectoryLevelType();
				if ((nextDirectoryLevelType == PathnameDirectoryLevelType.UP)
						|| (nextDirectoryLevelType == PathnameDirectoryLevelType.BACK)) {
					// TODO: should this take a stream!??!?
					throw new FileErrorException("WILD-INFERIORS may not be followed immediately by " + nextDirectoryLevelType + '.', null);
				}

				directoryLevel = next;
			}
		}
		return true;
	}

	protected String getDirectoryNamestring() {
		validateDirectory();

		final PathnameDirectoryComponent directoryComponent = directory.getDirectoryComponent();
		final List<PathnameDirectoryLevel> directoryLevels = directoryComponent.getDirectoryLevels();

		final StringBuilder stringBuilder = new StringBuilder();

		final PathnameDirectoryType pathnameDirectoryType = directoryComponent.getPathnameDirectoryType();

		switch (pathnameDirectoryType) {
			case ABSOLUTE:
				stringBuilder.append(File.separatorChar);
				break;
			case RELATIVE:
				if (directoryLevels.isEmpty()) {
					// #p"./"
					stringBuilder.append('.');
					stringBuilder.append(File.separatorChar);
				}
				break;
		}

		for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {
			final PathnameDirectoryLevelType directoryLevelType = directoryLevel.getDirectoryLevelType();
			switch (directoryLevelType) {
				case NULL:
					final String level = directoryLevel.getDirectoryLevel();
					stringBuilder.append(level);
					break;
				case WILD:
					stringBuilder.append(WILDCARD_STRING);
					break;
				case WILD_INFERIORS:
					stringBuilder.append(WILDCARD_INFERIORS_STRING);
					break;
				case UP:
					stringBuilder.append(BACK_UP_STRING);
					break;
				case BACK:
					stringBuilder.append(BACK_UP_STRING);
					break;
			}
			stringBuilder.append(File.separatorChar);
		}

		return stringBuilder.toString();
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link PathnameStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Building the {@link PathnameStruct#getUri()} value</li>
	 * <li>Constructing a new {@link PathnameStruct} with the built {@link URI} value</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String filePath = uri.toString();
		mv.visitLdcInsn(filePath);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_URI_NAME,
		                   GenerationConstants.JAVA_URI_CREATE_METHOD_NAME,
		                   GenerationConstants.JAVA_URI_CREATE_METHOD_DESC,
		                   false);
		final int uriStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, uriStore);

		mv.visitVarInsn(Opcodes.ALOAD, uriStore);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.PATHNAME_STRUCT_NAME,
		                   GenerationConstants.PATHNAME_STRUCT_TO_PATHNAME_URI_METHOD_NAME,
		                   GenerationConstants.PATHNAME_STRUCT_TO_PATHNAME_URI_METHOD_DESC,
		                   true);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printEscape.toJavaPBoolean()) {
			stringBuilder.append("#P");
		}
		stringBuilder.append('"');
		stringBuilder.append(getNamestring());
		stringBuilder.append('"');

		return stringBuilder.toString();
	}
}
