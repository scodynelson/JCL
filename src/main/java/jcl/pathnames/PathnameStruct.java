/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import jcl.classes.BuiltInClassStruct;
import jcl.types.Pathname;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public class PathnameStruct extends BuiltInClassStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5845491980801761678L;

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
	 * The {@link PathnameHost} value.
	 */
	protected final PathnameHost host;

	/**
	 * The {@link PathnameDevice} value.
	 */
	protected final PathnameDevice device;

	/**
	 * The {@link PathnameDirectory} value.
	 */
	protected final PathnameDirectory directory;

	/**
	 * The {@link PathnameName} value.
	 */
	protected final PathnameName name;

	/**
	 * The {@link PathnameType} value.
	 */
	protected final PathnameType type;

	/**
	 * The {@link PathnameVersion} value.
	 */
	protected final PathnameVersion version;

	/**
	 * The internal {@link URI} representation of the pathname.
	 */
	protected final URI uri;

	/**
	 * Public constructor.
	 *
	 * @param path
	 * 		the path to parse into the pathname object elements
	 */
	public PathnameStruct(final Path path) {
		this(path.toUri());
	}

	/**
	 * Public constructor.
	 *
	 * @param file
	 * 		the file to parse into the pathname object elements
	 */
	public PathnameStruct(final File file) {
		this(file.toURI());
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 */
	public PathnameStruct(final String pathname) {
		this(getURIFromPathname(pathname));
	}

	/**
	 * Public constructor.
	 *
	 * @param uri
	 * 		the {@link URI} to parse into the pathname object elements
	 */
	public PathnameStruct(final URI uri) {
		this(Pathname.INSTANCE, getHost(uri), getDevice(uri), getDirectory(uri), getName(uri), getType(uri), getVersion(), uri);
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
	public PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                      final PathnameName name, final PathnameType type, final PathnameVersion version) {
		this(Pathname.INSTANCE, host, device, directory, name, type, version, getURIFromComponents(host, device, directory, name, type, version));
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
	protected PathnameStruct(final Pathname pathnameType,
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
		final String uriPath = uri.getPath();
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

	/**
	 * Getter for pathname {@link #host} property.
	 *
	 * @return pathname {@link #host} property
	 */
	public PathnameHost getPathnameHost() {
		return host;
	}

	/**
	 * Getter for pathname {@link #device} property.
	 *
	 * @return pathname {@link #device} property
	 */
	public PathnameDevice getPathnameDevice() {
		return device;
	}

	/**
	 * Getter for pathname {@link #directory} property.
	 *
	 * @return pathname {@link #directory} property
	 */
	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	/**
	 * Getter for pathname {@link #name} property.
	 *
	 * @return pathname {@link #name} property
	 */
	public PathnameName getPathnameName() {
		return name;
	}

	/**
	 * Getter for pathname {@link #type} property.
	 *
	 * @return pathname {@link #type} property
	 */
	public PathnameType getPathnameType() {
		return type;
	}

	/**
	 * Getter for pathname {@link #version} property.
	 *
	 * @return pathname {@link #version} property
	 */
	public PathnameVersion getPathnameVersion() {
		return version;
	}

	/**
	 * Getter for pathname {@link #uri} property.
	 *
	 * @return pathname {@link #uri} property
	 */
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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(host)
		                            .append(device)
		                            .append(directory)
		                            .append(name)
		                            .append(type)
		                            .append(version)
		                            .append(uri)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final PathnameStruct rhs = (PathnameStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(host, rhs.host)
		                          .append(device, rhs.device)
		                          .append(directory, rhs.directory)
		                          .append(name, rhs.name)
		                          .append(type, rhs.type)
		                          .append(version, rhs.version)
		                          .append(uri, rhs.uri)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(host)
		                                                                .append(device)
		                                                                .append(directory)
		                                                                .append(name)
		                                                                .append(type)
		                                                                .append(version)
		                                                                .append(uri)
		                                                                .toString();
	}
}
