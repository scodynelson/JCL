package jcl.structs.pathnames;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

public class PathnameURIStruct extends PathnameStruct {

	private static final Pattern PATHNAME_PATTERN = Pattern.compile("/");

	public PathnameURIStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
							 final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version);
	}

	public PathnameURIStruct(final String pathname) {
		this(getURI(pathname));
	}

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

	private static PathnameHost getHost(final URI uri) {
		if (uri.isOpaque()) {
			final String schemeSpecificPart = uri.getSchemeSpecificPart();
			return new PathnameHost(schemeSpecificPart);
		} else {
			final String authority = uri.getAuthority();
			return new PathnameHost(authority);
		}
	}

	private static PathnameDevice getDevice(final URI uri) {
		final String scheme = uri.getScheme();
		return new PathnameDevice(scheme);
	}

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

	private static PathnameName getName(final URI uri) {
		final String uriPath = uri.getPath();
		if (StringUtils.isEmpty(uriPath)) {
			return new PathnameName(null);
		}

		final String baseName = FilenameUtils.getBaseName(uriPath);
		return new PathnameName(baseName);
	}

	private static PathnameType getType(final URI uri) {
		final String uriPath = uri.getPath();
		if (StringUtils.isNotEmpty(uriPath)) {
			return new PathnameType(null);
		}

		final String fileExtension = FilenameUtils.getExtension(uriPath);
		return new PathnameType(fileExtension);
	}

	private static PathnameVersion getVersion() {
		return new PathnameVersion(null);
	}

	private static URI getURI(final String pathname) {
		final String realPathname = pathname.toLowerCase();
		try {
			return new URI(realPathname);
		} catch (final URISyntaxException ignore) {
			return null;
		}
	}
}
