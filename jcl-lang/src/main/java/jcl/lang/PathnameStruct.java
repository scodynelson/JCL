package jcl.lang;

import java.net.URI;
import java.nio.file.Path;

import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public interface PathnameStruct extends LispStruct {

	Path getPath();

	boolean exists();

	/**
	 * Getter for pathname {@link PathnameHost} value.
	 *
	 * @return pathname {@link PathnameHost} value
	 */
	PathnameHost getPathnameHost();

	/**
	 * Getter for pathname {@link PathnameDevice} value.
	 *
	 * @return pathname {@link PathnameDevice} value
	 */
	PathnameDevice getPathnameDevice();

	/**
	 * Getter for pathname {@link PathnameDirectory} value.
	 *
	 * @return pathname {@link PathnameDirectory} value
	 */
	PathnameDirectory getPathnameDirectory();

	/**
	 * Getter for pathname {@link PathnameName} value.
	 *
	 * @return pathname {@link PathnameName} value
	 */
	PathnameName getPathnameName();

	/**
	 * Getter for pathname {@link PathnameType} value.
	 *
	 * @return pathname {@link PathnameType} value
	 */
	PathnameType getPathnameType();

	/**
	 * Getter for pathname {@link PathnameVersion} value.
	 *
	 * @return pathname {@link PathnameVersion} value
	 */
	PathnameVersion getPathnameVersion();

	/**
	 * Getter for pathname {@link URI} value.
	 *
	 * @return pathname {@link URI} value
	 */
	URI getUri();

	String getNamestring();

}
