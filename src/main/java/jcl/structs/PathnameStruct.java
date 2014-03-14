package jcl.structs;

import jcl.classes.BuiltInClassStruct;
import jcl.structs.conditions.exceptions.FileErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.types.Pathname;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

/**
 * The {@code PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public abstract class PathnameStruct extends BuiltInClassStruct {

	protected final PathnameHost host;
	protected final PathnameDevice device;
	protected final PathnameDirectory directory;
	protected final PathnameName name;
	protected final PathnameType type;
	protected final PathnameVersion version;

	private static final Logger LOGGER = LoggerFactory.getLogger(PathnameStruct.class);

	/**
	 * Protected constructor.
	 *
	 * @param host      the pathname host
	 * @param device    the pathname device
	 * @param directory the pathname directory
	 * @param name      the pathname name
	 * @param type      the pathname type
	 * @param version   the pathname version
	 */
	protected PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		this(Pathname.INSTANCE, host, device, directory, name, type, version);
	}

	/**
	 * Protected constructor.
	 *
	 * @param pathnameType the pathname structure type
	 * @param host         the pathname host
	 * @param device       the pathname device
	 * @param directory    the pathname directory
	 * @param name         the pathname name
	 * @param type         the pathname type
	 * @param version      the pathname version
	 */
	protected PathnameStruct(final Pathname pathnameType,
							 final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(pathnameType, null, null);
		this.host = host;
		this.device = device;
		this.directory = directory;
		this.name = name;
		this.type = type;
		this.version = version;
	}

	/**
	 * Getter for pathname host property.
	 *
	 * @return pathname host property
	 */
	public PathnameHost getPathnameHost() {
		return host;
	}

	/**
	 * Getter for pathname device property.
	 *
	 * @return pathname device property
	 */
	public PathnameDevice getPathnameDevice() {
		return device;
	}

	/**
	 * Getter for pathname directory property.
	 *
	 * @return pathname directory property
	 */
	public PathnameDirectory getPathnameDirectory() {
		return directory;
	}

	/**
	 * Getter for pathname name property.
	 *
	 * @return pathname name property
	 */
	public PathnameName getPathnameName() {
		return name;
	}

	/**
	 * Getter for pathname type property.
	 *
	 * @return pathname type property
	 */
	public PathnameType getPathnameType() {
		return type;
	}

	/**
	 * Getter for pathname version property.
	 *
	 * @return pathname version property
	 */
	public PathnameVersion getPathnameVersion() {
		return version;
	}

	/**
	 * This method determines if the provided path is a URI.
	 *
	 * @param path the path to test
	 * @return whether or not the provide path is a URI
	 */
	private static boolean isURI(final String path) {
		try {
			final URI uri = new URI(path);
			return uri.isAbsolute();
		} catch (final URISyntaxException use) {
			LOGGER.trace("Provided path cannot be parsed as a URI: {}", path, use);
			return false;
		}
	}

	@Override
	public String toString() {
		return "PathnameStruct{"
				+ "host=" + host
				+ ", device=" + device
				+ ", directory=" + directory
				+ ", name=" + name
				+ ", type=" + type
				+ ", version=" + version
				+ '}';
	}

	// BUILDERS

	/**
	 * This factory method builds and returns a pathname with the provided {@code pathname} parsed as its elements.
	 *
	 * @param pathname the pathname string to parse into the pathname object elements
	 * @return the constructed pathname with constructed elements
	 * @throws URISyntaxException if the provided pathname is determined to be a URI, but cannot be parsed as one
	 *                            NOTE: THIS SHOULD NEVER HAPPEN BUT WE THROW THIS FOR SAFETY CASES
	 */
	public static PathnameStruct buildPathname(final String pathname) throws URISyntaxException {
		if (isURI(pathname)) {
			return new PathnameURIStruct(pathname);
		} else {
			return new PathnameFileStruct(pathname);
		}
	}

	/**
	 * This factory method builds and returns a pathname of the {@code structType} with the provided {@code host},
	 * {@code device},{@code directory}, {@code name},{@code type},{@code version} as its elements.
	 *
	 * @param host       the pathname host
	 * @param device     the pathname device
	 * @param directory  the pathname directory
	 * @param name       the pathname name
	 * @param type       the pathname type
	 * @param version    the pathname version
	 * @param structType the type of pathname to build
	 * @return the constructed pathname with the provided elements
	 */
	public static PathnameStruct buildPathname(final PathnameHost host, final PathnameDevice device,
											   final PathnameDirectory directory, final PathnameName name,
											   final PathnameType type, final PathnameVersion version,
											   final PathnameStructType structType) {
		switch (structType) {
			case FILE:
				return new PathnameFileStruct(host, device, directory, name, type, version);
			case URI:
				return new PathnameURIStruct(host, device, directory, name, type, version);
			default:
				throw new SimpleErrorException("Unsupported pathname structure cannot be constructed: " + structType + '.');
		}
	}

	// PathnameStruct Element Objects

	/**
	 * The {@code PathnameHost} is the object representation of the 'host' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameHost {

		private final String host;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameHost() {
			host = null;
			componentType = PathnameComponentType.UNSPECIFIC;
		}

		/**
		 * Public constructor.
		 *
		 * @param host the pathname host
		 */
		public PathnameHost(final String host) {
			this.host = host;

			if (StringUtils.isEmpty(host)) {
				componentType = PathnameComponentType.NIL;
			} else {
				componentType = null;
			}
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname host component type
		 */
		public PathnameHost(final PathnameComponentType componentType) {
			host = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname host value.
		 *
		 * @return pathname host value
		 */
		public String getHost() {
			return host;
		}

		/**
		 * Getter for pathname host component type.
		 *
		 * @return pathname host component type
		 */
		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameHost{"
					+ "host=" + host
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDevice} is the object representation of the 'device' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDevice {

		private final String device;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameDevice() {
			device = null;
			componentType = PathnameComponentType.UNSPECIFIC;
		}

		/**
		 * Public constructor.
		 *
		 * @param device the pathname device
		 */
		public PathnameDevice(final String device) {
			this.device = device;

			if (StringUtils.isEmpty(device)) {
				componentType = PathnameComponentType.NIL;
			} else if ("*".equals(device)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname device component type
		 */
		public PathnameDevice(final PathnameComponentType componentType) {
			device = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname device value.
		 *
		 * @return pathname device value
		 */
		public String getDevice() {
			return device;
		}

		/**
		 * Getter for pathname device component type.
		 *
		 * @return pathname device component type
		 */
		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameDevice{"
					+ "device=" + device
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDirectory} is the object representation of the 'directory' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDirectory {

		private final PathnameDirectoryComponent directoryComponent;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameDirectory() {
			directoryComponent = null;
			componentType = PathnameComponentType.UNSPECIFIC;
		}

		/**
		 * Public constructor.
		 *
		 * @param directoryComponent the pathname directory component
		 */
		public PathnameDirectory(final PathnameDirectoryComponent directoryComponent) {
			this.directoryComponent = directoryComponent;

			if (directoryComponent == null) {
				componentType = PathnameComponentType.NIL;
			} else {
				componentType = null;
			}
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname directory component type
		 */
		public PathnameDirectory(final PathnameComponentType componentType) {
			directoryComponent = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname directory component value.
		 *
		 * @return pathname directory component value
		 */
		public PathnameDirectoryComponent getDirectoryComponent() {
			return directoryComponent;
		}

		/**
		 * Getter for pathname directory component type.
		 *
		 * @return pathname directory component type
		 */
		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameDirectory{"
					+ "directoryComponent=" + directoryComponent
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDirectoryComponent} is the object representation of a directory component of the 'directory'
	 * element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDirectoryComponent {

		private final PathnameDirectoryType pathnameDirectoryType;
		private final List<PathnameDirectoryLevel> directoryLevels;

		/**
		 * Public constructor.
		 *
		 * @param directoryLevels       the pathname directory levels
		 * @param pathnameDirectoryType the pathname directory type (ABSOLUTE or RELATIVE)
		 */
		public PathnameDirectoryComponent(final PathnameDirectoryType pathnameDirectoryType, final List<PathnameDirectoryLevel> directoryLevels) {
			if (CollectionUtils.isNotEmpty(directoryLevels) && (pathnameDirectoryType == PathnameDirectoryType.ABSOLUTE)) {
				final PathnameDirectoryLevel firstElement = directoryLevels.get(0);
				if ((firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.BACK) ||
						(firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.UP)) {
					throw new FileErrorException(":ABSOLUTE must not be followed by :BACK or :UP.");
				}
			}

			this.pathnameDirectoryType = pathnameDirectoryType;
			this.directoryLevels = directoryLevels;
		}

		/**
		 * Getter for pathname directory type.
		 *
		 * @return pathname directory type
		 */
		public PathnameDirectoryType getPathnameDirectoryType() {
			return pathnameDirectoryType;
		}

		/**
		 * Getter for pathname directory levels.
		 *
		 * @return pathname directory levels
		 */
		public List<PathnameDirectoryLevel> getDirectoryLevels() {
			return directoryLevels;
		}

		@Override
		public String toString() {
			return "PathnameDirectory{"
					+ "directoryLevels=" + directoryLevels
					+ ", pathnameDirectoryType=" + pathnameDirectoryType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDirectoryLevel} is the object representation of a specific directory level of the 'directory'
	 * element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDirectoryLevel {

		private final String directoryLevel;
		private PathnameDirectoryLevelType directoryLevelType;

		/**
		 * Public constructor.
		 *
		 * @param directoryLevel the directory level value
		 */
		public PathnameDirectoryLevel(final String directoryLevel) {
			if (StringUtils.isEmpty(directoryLevel)) {
				throw new FileErrorException("Directory level value cannot be null or empty.");
			}

			this.directoryLevel = directoryLevel;
			directoryLevelType = null;
		}

		/**
		 * Public constructor.
		 *
		 * @param directoryLevelType the directory level type (WILD, BACK, or UP)
		 */
		public PathnameDirectoryLevel(final PathnameDirectoryLevelType directoryLevelType) {
			directoryLevel = null;
			this.directoryLevelType = directoryLevelType;
		}

		/**
		 * Getter for pathname directory level value.
		 *
		 * @return pathname directory level value
		 */
		public String getDirectoryLevel() {
			return directoryLevel;
		}

		/**
		 * Getter for pathname directory level type.
		 *
		 * @return pathname directory level type
		 */
		public PathnameDirectoryLevelType getDirectoryLevelType() {
			return directoryLevelType;
		}

		/**
		 * Setter for pathname directory level type.
		 *
		 * @param directoryLevelType new directoryLevelType value
		 */
		public void setDirectoryLevelType(final PathnameDirectoryLevelType directoryLevelType) {
			this.directoryLevelType = directoryLevelType;
		}

		@Override
		public String toString() {
			return "PathnameDirectoryLevel{"
					+ "directoryLevel='" + directoryLevel + '\''
					+ ", directoryLevelType=" + directoryLevelType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname' type.
	 */
	public enum PathnameDirectoryType {
		ABSOLUTE(":ABSOLUTE"),
		RELATIVE(":RELATIVE");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the directory type
		 */
		PathnameDirectoryType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for directory type value.
		 *
		 * @return directory type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameDirectoryLevelType} is the enumeration of the directory level type of a 'directory' level
	 * element of a Lisp 'pathname' type.
	 * NOTE: This implementation does NOT support WildInferiors. Period.
	 */
	public enum PathnameDirectoryLevelType {
		WILD(":WILD"),
		BACK(":BACK"),
		UP(":UP");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the directory level type
		 */
		PathnameDirectoryLevelType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for directory level type value.
		 *
		 * @return directory level type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameName} is the object representation of the 'name' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameName {

		private final String name;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameName() {
			name = null;
			componentType = PathnameComponentType.UNSPECIFIC;
		}

		/**
		 * Public constructor.
		 *
		 * @param name the pathname name
		 */
		public PathnameName(final String name) {
			this.name = name;

			if (StringUtils.isEmpty(name)) {
				componentType = PathnameComponentType.NIL;
			} else if ("*".equalsIgnoreCase(name)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname name component type
		 */
		public PathnameName(final PathnameComponentType componentType) {
			name = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname name value.
		 *
		 * @return pathname name value
		 */
		public String getName() {
			return name;
		}

		/**
		 * Getter for pathname name component type.
		 *
		 * @return pathname name component type
		 */
		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameName{"
					+ "name=" + name
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameType} is the object representation of the 'type' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameType {

		private final String type;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameType() {
			type = null;
			componentType = PathnameComponentType.UNSPECIFIC;
		}

		/**
		 * Public constructor.
		 *
		 * @param type the pathname type
		 */
		public PathnameType(final String type) {
			this.type = type;

			if (StringUtils.isEmpty(type)) {
				componentType = PathnameComponentType.NIL;
			} else if ("*".equalsIgnoreCase(type)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname type component type
		 */
		public PathnameType(final PathnameComponentType componentType) {
			type = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname type value.
		 *
		 * @return pathname type value
		 */
		public String getType() {
			return type;
		}

		/**
		 * Getter for pathname type component type.
		 *
		 * @return pathname type component type
		 */
		public PathnameComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameType{"
					+ "type=" + type
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameVersion} is the object representation of the 'version' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameVersion {

		private final Integer version;
		private final PathnameVersionComponentType componentType;

		/**
		 * Public constructor.
		 */
		public PathnameVersion() {
			version = null;
			componentType = PathnameVersionComponentType.NEWEST;
		}

		/**
		 * Public constructor.
		 *
		 * @param version the pathname version
		 */
		public PathnameVersion(final Integer version) {
			if ((version == null) || (version < 1)) {
				throw new FileErrorException("Version value cannot be null or less than 1.");
			}

			this.version = version;
			componentType = PathnameVersionComponentType.NEWEST;
		}

		/**
		 * Public constructor.
		 *
		 * @param componentType pathname version component type
		 */
		public PathnameVersion(final PathnameVersionComponentType componentType) {
			version = null;
			this.componentType = componentType;
		}

		/**
		 * Getter for pathname version value.
		 *
		 * @return pathname version value
		 */
		public Integer getVersion() {
			return version;
		}

		/**
		 * Getter for pathname version component type.
		 *
		 * @return pathname version component type
		 */
		public PathnameVersionComponentType getComponentType() {
			return componentType;
		}

		@Override
		public String toString() {
			return "PathnameVersion{"
					+ "version=" + version
					+ ", componentType=" + componentType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameComponentType} is the enumeration of the type of a component element of a Lisp 'pathname' type.
	 * TODO: support both "wild" singular and plural. right now we only support plural: '*' vs '?'
	 * TODO: also for UNIX, support character groupings and negation
	 * NOTE: should 'WILD' things eventually be known as 'GLOB' things???
	 * http://en.wikipedia.org/wiki/Glob_(programming)
	 */
	public enum PathnameComponentType {
		UNSPECIFIC(":UNSPECIFIC"),
		WILD(":WILD"),
		NIL("NIL");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the component type
		 */
		PathnameComponentType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for component type value.
		 *
		 * @return component type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameVersionComponentType} is the enumeration of the type of a component element of the version element
	 * of a Lisp 'pathname' type.
	 */
	public enum PathnameVersionComponentType {
		UNSPECIFIC(":UNSPECIFIC"),
		WILD(":WILD"),
		NIL("NIL"),
		NEWEST(":NEWEST"),
		OLDEST(":OLDEST");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the version component type
		 */
		PathnameVersionComponentType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for version component type value.
		 *
		 * @return version component type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameCase} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
	 */
	public enum PathnameCaseType {
		COMMON(":COMMON"),
		LOCAL(":LOCAL");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the case type
		 */
		PathnameCaseType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for case type value.
		 *
		 * @return case type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameStructType} is the enumeration of the structure type of a Lisp 'pathname' type.
	 */
	public enum PathnameStructType {
		FILE(":FILE"),
		URI(":URI");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the pathname structure type
		 */
		PathnameStructType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for pathname structure type value.
		 *
		 * @return pathname structure type value
		 */
		public String getValue() {
			return value;
		}
	}
}
