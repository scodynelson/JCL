package jcl.structs.pathnames;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The {@code PathnameURIStruct} is the uri-type object representation of a Lisp 'pathname' type.
 */
public class PathnameURIStruct extends PathnameStruct {

	private static final Pattern PATHNAME_PATTERN = Pattern.compile("/");

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
	public PathnameURIStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version);
	}

	/**
	 * Public constructor.
	 *
	 * @param pathname the pathname string to parse into the pathname object elements
	 * @throws URISyntaxException if the provided {@code pathname} cannot be parsed as a URI
	 */
	public PathnameURIStruct(final String pathname) throws URISyntaxException {
		this(getURI(pathname));
	}

	/**
	 * Public constructor.
	 *
	 * @param path the path to parse into the pathname object elements
	 */
	public PathnameURIStruct(final Path path) {
		this(path.toUri());
	}

	/**
	 * Public constructor.
	 *
	 * @param uri the uri to parse into the pathname object elements
	 */
	public PathnameURIStruct(final URI uri) {
		this(getHost(uri), getDevice(uri), getDirectory(uri), getName(uri), getType(uri), getVersion());
	}

	@Override
	public String toString() {
		return "PathnameURIStruct{"
				+ "host=" + host
				+ ", device=" + device
				+ ", directory=" + directory
				+ ", name=" + name
				+ ", type=" + type
				+ ", version=" + version
				+ '}';
	}

	/**
	 * This method gets the pathname host.
	 *
	 * @param uri the uri to parse into the pathname host
	 * @return the pathname host
	 */
	private static PathnameHost getHost(final URI uri) {
		if (uri.isOpaque()) {
			final String schemeSpecificPart = uri.getSchemeSpecificPart();
			return new PathnameHost(schemeSpecificPart);
		} else {
			final String authority = uri.getAuthority();
			return new PathnameHost(authority);
		}
	}

	/**
	 * This method gets the pathname device.
	 *
	 * @param uri the uri to parse into the pathname device
	 * @return the pathname device
	 */
	private static PathnameDevice getDevice(final URI uri) {
		final String scheme = uri.getScheme();
		return new PathnameDevice(scheme);
	}

	/**
	 * This method gets the pathname directory.
	 *
	 * @param uri the uri to parse into the pathname directory
	 * @return the pathname directory
	 */
	private static PathnameDirectory getDirectory(final URI uri) {
		final String uriPath = uri.getPath();

		final List<String> directoryStrings;
		if (StringUtils.isNotEmpty(uriPath)) {
			final String directoryPath = FilenameUtils.getFullPathNoEndSeparator(uriPath);
			final String[] tokens = PATHNAME_PATTERN.split(directoryPath);

			directoryStrings = new ArrayList<>(Arrays.asList(tokens));
		} else {
			directoryStrings = Collections.emptyList();
		}

		// add query, if any, to directory path
		if (uri.getQuery() != null) {
			directoryStrings.add('?' + uri.getQuery());
		}

		// add fragment, if any, to directory path
		if (uri.getFragment() != null) {
			directoryStrings.add('#' + uri.getFragment());
		}

		if (uri.isAbsolute()) {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.ABSOLUTE);
		} else {
			return new PathnameDirectory(directoryStrings, PathnameDirectoryType.RELATIVE);
		}
	}

	/**
	 * This method gets the pathname name.
	 *
	 * @param uri the uri to parse into the pathname name
	 * @return the pathname name
	 */
	private static PathnameName getName(final URI uri) {
		final String uriPath = uri.getPath();
		if (StringUtils.isEmpty(uriPath)) {
			return new PathnameName(null);
		}

		final String baseName = FilenameUtils.getBaseName(uriPath);
		return new PathnameName(baseName);
	}

	/**
	 * This method gets the pathname type.
	 *
	 * @param uri the uri to parse into the pathname type
	 * @return the pathname type
	 */
	private static PathnameType getType(final URI uri) {
		final String uriPath = uri.getPath();
		if (StringUtils.isNotEmpty(uriPath)) {
			return new PathnameType(null);
		}

		final String fileExtension = FilenameUtils.getExtension(uriPath);
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
	 * This method gets a URI from the provided {@code pathname}.
	 *
	 * @param pathname the pathname string to convert to a URI
	 * @return the URI value of the provided {@code pathname}
	 * @throws URISyntaxException if the provided {@code pathname} cannot be parsed as a URI
	 */
	private static URI getURI(final String pathname) throws URISyntaxException {
		final String realPathname = pathname.toLowerCase();
		return new URI(realPathname);
	}
}
