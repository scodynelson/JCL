package jcl.pathnames;

import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.types.Pathname;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;

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
}