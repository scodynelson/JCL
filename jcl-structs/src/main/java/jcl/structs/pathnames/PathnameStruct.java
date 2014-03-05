package jcl.structs.pathnames;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.types.pathnames.Pathname;
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
		super(Pathname.INSTANCE, null, null);
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
		 *
		 * @param host the pathname host
		 */
		public PathnameHost(final String host) {
			this.host = host;

			if (host == null) {
				componentType = PathnameComponentType.NIL;
			} else if (host.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else {
				componentType = null;
			}
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
	 * The {@code PathnameHost} is the object representation of the 'device' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDevice {

		private final String device;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 *
		 * @param device the pathname device
		 */
		public PathnameDevice(final String device) {
			this.device = device;

			if (device == null) {
				componentType = PathnameComponentType.NIL;
			} else if (device.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(device)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
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
	 * The {@code PathnameHost} is the object representation of the 'directory' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameDirectory {

		private final List<String> directory;
		private final PathnameDirectoryType pathnameDirectoryType;

		/**
		 * Public constructor.
		 *
		 * @param directory             the pathname directory
		 * @param pathnameDirectoryType the pathname directory type (ABSOLUTE or RELATIVE)
		 */
		public PathnameDirectory(final List<String> directory, final PathnameDirectoryType pathnameDirectoryType) {
			this.directory = directory;
			this.pathnameDirectoryType = pathnameDirectoryType;
		}

		/**
		 * Getter for pathname directory value.
		 *
		 * @return pathname directory value
		 */
		public List<String> getDirectory() {
			return directory;
		}

		/**
		 * Getter for pathname directory type.
		 *
		 * @return pathname directory type
		 */
		public PathnameDirectoryType getPathnameDirectoryType() {
			return pathnameDirectoryType;
		}

		@Override
		public String toString() {
			return "PathnameDirectory{"
					+ "directory=" + directory
					+ ", pathnameDirectoryType=" + pathnameDirectoryType
					+ '}';
		}
	}

	/**
	 * The {@code PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname' type.
	 */
	public enum PathnameDirectoryType {
		ABSOLUTE,
		RELATIVE
	}

	/**
	 * The {@code PathnameDirectoryDirectionType} is the enumeration of the direction type of an individual 'directory'
	 * element of a Lisp 'pathname' type.
	 */
	public enum PathnameDirectoryDirectionType {

		BACK(":BACK"),
		UP(":UP");

		private final String value;

		/**
		 * Constructor.
		 *
		 * @param value value of the directory direction type
		 */
		PathnameDirectoryDirectionType(final String value) {
			this.value = value;
		}

		/**
		 * Getter for directory direction type value.
		 *
		 * @return directory direction type value
		 */
		public String getValue() {
			return value;
		}
	}

	/**
	 * The {@code PathnameHost} is the object representation of the 'name' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameName {

		private final String name;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 *
		 * @param name the pathname name
		 */
		public PathnameName(final String name) {
			this.name = name;

			if (name == null) {
				componentType = PathnameComponentType.NIL;
			} else if (name.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(name)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
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
	 * The {@code PathnameHost} is the object representation of the 'type' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameType {

		private final String type;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 *
		 * @param type the pathname type
		 */
		public PathnameType(final String type) {
			this.type = type;

			if (type == null) {
				componentType = PathnameComponentType.NIL;
			} else if (type.isEmpty()) {
				componentType = PathnameComponentType.UNSPECIFIC;
			} else if ("*".equalsIgnoreCase(type)) {
				componentType = PathnameComponentType.WILD;
			} else {
				componentType = null;
			}
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
	 * The {@code PathnameHost} is the object representation of the 'version' element of a Lisp 'pathname' type.
	 */
	public static final class PathnameVersion {

		private final Integer version;
		private final PathnameComponentType componentType;

		/**
		 * Public constructor.
		 *
		 * @param version the pathname version
		 */
		public PathnameVersion(final Integer version) {
			this.version = version;

			if (version == null) {
				componentType = PathnameComponentType.NEWEST;
			} else {
				componentType = PathnameComponentType.UNSPECIFIC;
			}
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
		public PathnameComponentType getComponentType() {
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
	 * The {@code PathnameDirectoryType} is the enumeration of the type of the 'directory' element of a Lisp 'pathname' type.
	 */
	public enum PathnameComponentType {
		UNSPECIFIC,
		WILD,
		NEWEST,
		OLDEST,
		PREVIOUS,
		INSTALLED,
		NIL
	}

	/**
	 * The {@code PathnameCase} is the enumeration of the case types to parse the elements of a Lisp 'pathname' type.
	 */
	public enum PathnameCase {
		COMMON,
		LOCAL
	}

	/**
	 * The {@code PathnameStructType} is the enumeration of the structure type of a Lisp 'pathname' type.
	 */
	public enum PathnameStructType {
		FILE,
		URI
	}
}
