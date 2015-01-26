/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.printer.PrinterVariables;
import jcl.symbols.BooleanStruct;
import jcl.types.Pathname;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public abstract class PathnameStruct extends BuiltInClassStruct {

	private static final long serialVersionUID = -5845491980801761678L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PathnameStruct.class);
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
	protected PathnameStruct(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                         final PathnameName name, final PathnameType type, final PathnameVersion version) {
		this(Pathname.INSTANCE, host, device, directory, name, type, version);
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
	 * Builds and returns a pathname with the provided {@code pathname} parsed as its elements.
	 *
	 * @param pathname
	 * 		the pathname string to parse into the pathname object elements
	 *
	 * @return the constructed pathname with constructed elements
	 *
	 * @throws URISyntaxException
	 * 		if the provided pathname is determined to be a URI, but cannot be parsed as one
	 * 		NOTE: THIS SHOULD NEVER HAPPEN BUT WE THROW THIS FOR SAFETY CASES
	 */
	public static PathnameStruct buildPathname(final String pathname) throws URISyntaxException {
		if (isURI(pathname)) {
			return new PathnameURIStruct(pathname);
		} else {
			return new PathnameFileStruct(pathname);
		}
	}

	/**
	 * Determines if the provided path is a URI.
	 *
	 * @param path
	 * 		the path to test
	 *
	 * @return whether or not the provide path is a URI
	 */
	private static boolean isURI(final String path) {
		try {
			final URI uri = new URI(path);
			return uri.isAbsolute();
		} catch (final URISyntaxException use) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Provided path cannot be parsed as a URI: {}", path, use);
			}
			return false;
		}
	}

	/**
	 * Builds and returns a pathname of the {@code structType} with the provided {@code host}, {@code device}, {@code
	 * directory}, {@code name}, {@code type}, {@code version} as its elements.
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
	 * @param structType
	 * 		the type of pathname to build
	 *
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

	// BUILDERS

	@Override
	public String printStruct() {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printEscape.booleanValue()) {
			stringBuilder.append("#P");
		}
		stringBuilder.append('"');

		final String deviceString = device.getDevice();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append(deviceString);
			stringBuilder.append(':');
		}

		final List<PathnameDirectoryLevel> directoryLevels = directory.getDirectoryComponent().getDirectoryLevels();
		for (final PathnameDirectoryLevel directoryLevel : directoryLevels) {
			stringBuilder.append(File.separatorChar);
			stringBuilder.append(directoryLevel.getDirectoryLevel());
		}

		final String nameString = name.getName();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append(nameString);
		}

		final String typeString = type.getType();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append('.');
			stringBuilder.append(typeString);
		}

		final String versionString = version.getVersion().toString();
		if (StringUtils.isNoneEmpty(deviceString)) {
			stringBuilder.append('.');
			stringBuilder.append(versionString);
		}

		stringBuilder.append('"');

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
