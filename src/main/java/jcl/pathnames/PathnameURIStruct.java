/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 * The {@link PathnameURIStruct} is the uri-type object representation of a Lisp 'pathname' type.
 */
class PathnameURIStruct extends PathnameStruct {

	private static final long serialVersionUID = 6269284949167744555L;

	/**
	 * {@link Pattern} used to parse pathname URI paths.
	 */
	private static final Pattern PATHNAME_PATTERN = Pattern.compile("/");

	/**
	 * Package constructor.
	 *
	 * @param path
	 * 		the path to parse into the pathname object elements
	 */
	PathnameURIStruct(final Path path) {
		this(path.toUri());
	}

	/**
	 * Package constructor.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 *
	 * @throws URISyntaxException
	 * 		if the provided {@code pathname} cannot be parsed as a URI
	 */
	PathnameURIStruct(final String pathname) throws URISyntaxException {
		this(getURI(pathname));
	}

	/**
	 * Package constructor.
	 *
	 * @param uri
	 * 		the uri to parse into the pathname object elements
	 */
	PathnameURIStruct(final URI uri) {
		this(getHost(uri), getDevice(uri), getDirectory(uri), getName(uri), getType(uri), getVersion());
	}

	/**
	 * Package constructor.
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
	PathnameURIStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                  final PathnameName name, final PathnameType type, final PathnameVersion version) {
		super(host, device, directory, name, type, version);
	}

	/**
	 * Gets the pathname host.
	 *
	 * @param uri
	 * 		the uri to parse into the pathname host
	 *
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
	 * Gets the pathname device.
	 *
	 * @param uri
	 * 		the uri to parse into the pathname device
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
	 * 		the uri to parse into the pathname directory
	 *
	 * @return the pathname directory
	 */
	private static PathnameDirectory getDirectory(final URI uri) {
		final String uriPath = uri.getPath();

		final List<String> directoryStrings = new ArrayList<>();
		if (StringUtils.isNotEmpty(uriPath)) {
			final String directoryPath = FilenameUtils.getFullPathNoEndSeparator(uriPath);
			final String[] tokens = PATHNAME_PATTERN.split(directoryPath);

			directoryStrings.addAll(Arrays.asList(tokens));
		}

		final boolean isAbsolute = uri.isAbsolute();
		final PathnameDirectoryType directoryType = isAbsolute ? PathnameDirectoryType.ABSOLUTE : PathnameDirectoryType.RELATIVE;

		final List<PathnameDirectoryLevel> directoryLevels = new ArrayList<>(directoryStrings.size());

		for (final String directoryString : directoryStrings) {
			final PathnameDirectoryLevel directoryLevel = new PathnameDirectoryLevel(directoryString);
			directoryLevels.add(directoryLevel);
		}

		// add query, if any, to directory path
		final String query = uri.getQuery();
		if (query != null) {
			directoryLevels.add(new PathnameDirectoryLevel("?"));
			if (!query.isEmpty()) {
				directoryLevels.add(new PathnameDirectoryLevel(query));
			}
		}

		// add fragment, if any, to directory path
		final String fragment = uri.getFragment();
		if (fragment != null) {
			directoryLevels.add(new PathnameDirectoryLevel("#"));
			if (!fragment.isEmpty()) {
				directoryLevels.add(new PathnameDirectoryLevel(fragment));
			}
		}

		final PathnameDirectoryComponent pathnameDirectoryComponent = new PathnameDirectoryComponent(directoryType, directoryLevels);
		return new PathnameDirectory(pathnameDirectoryComponent);
	}

	/**
	 * Gets the pathname name.
	 *
	 * @param uri
	 * 		the uri to parse into the pathname name
	 *
	 * @return the pathname name
	 */
	private static PathnameName getName(final URI uri) {
		final String uriPath = uri.getPath();
		final String baseName = FilenameUtils.getBaseName(uriPath);
		return new PathnameName(baseName);
	}

	/**
	 * Gets the pathname type.
	 *
	 * @param uri
	 * 		the uri to parse into the pathname type
	 *
	 * @return the pathname type
	 */
	private static PathnameType getType(final URI uri) {
		final String uriPath = uri.getPath();
		final String fileExtension = FilenameUtils.getExtension(uriPath);
		return new PathnameType(fileExtension);
	}

	/**
	 * Gets the pathname version.
	 *
	 * @return the pathname version
	 */
	private static PathnameVersion getVersion() {
		return new PathnameVersion(PathnameVersionComponentType.UNSPECIFIC);
	}

	/**
	 * Gets a URI from the provided {@code pathname}.
	 *
	 * @param pathname
	 * 		the pathname string to convert to a URI
	 *
	 * @return the URI value of the provided {@code pathname}
	 *
	 * @throws URISyntaxException
	 * 		if the provided {@code pathname} cannot be parsed as a URI
	 */
	private static URI getURI(final String pathname) throws URISyntaxException {
		final String realPathname = pathname.toLowerCase(Locale.getDefault());
		return new URI(realPathname);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
