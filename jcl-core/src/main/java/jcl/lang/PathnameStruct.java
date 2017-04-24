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

	@Override
	default boolean equal(final LispStruct object) {
		// TODO: clean this up!!!
		if (eq(object)) {
			return true;
		}
		if (object instanceof PathnameStruct) {
			final PathnameStruct p = (PathnameStruct) object;
//			if (Utilities.isPlatformWindows) {
//				if (!host.equalp(p.host)) {
//					return false;
//				}
//				if (!device.equalp(p.device)) {
//					return false;
//				}
//				if (!directory.equalp(p.directory)) {
//					return false;
//				}
//				if (!name.equalp(p.name)) {
//					return false;
//				}
//				if (!type.equalp(p.type)) {
//					return false;
//				}
//				// Ignore version component.
//				//if (!version.equalp(p.version))
//				//    return false;
//			} else {
			// Unix.
			if (!getPathnameHost().equals(p.getPathnameHost())) {
				return false;
			}
			if (!getPathnameDevice().equals(p.getPathnameDevice())) {
				return false;
			}
			if (!getPathnameDirectory().equals(p.getPathnameDirectory())) {
				return false;
			}
			if (!getPathnameName().equals(p.getPathnameName())) {
				return false;
			}
			if (!getPathnameType().equals(p.getPathnameType())) {
				return false;
			}
			// Ignore version component.
			//if (!getPathnameVersion.equals(p.getPathnameVersion))
			//    return false;
//			}
			return true;
		}
		return false;
	}
}
