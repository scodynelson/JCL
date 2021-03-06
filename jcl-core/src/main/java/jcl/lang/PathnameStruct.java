package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.PathnameStructImpl;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link PathnameStruct} is the object representation of a Lisp 'pathname' type.
 */
public interface PathnameStruct extends LispStruct {

	LispStruct pathnameHost();
	LispStruct pathnameDevice();
	LispStruct pathnameDirectory();
	LispStruct pathnameName();
	LispStruct pathnameType();
	LispStruct pathnameVersion();

	String namestring();

	StringStruct directoryNamestring();

	LispStruct fileNamestring();

	BooleanStruct wildPathnameP(final LispStruct fieldKey);

	@Override
	default boolean equal(final LispStruct object) {
		if (this == object) {
			return true;
		}
		if (object instanceof PathnameStruct) {
			final PathnameStruct p = (PathnameStruct) object;
			if (SystemUtils.IS_OS_WINDOWS) {
				if (!pathnameHost().equalp(p.pathnameHost())) {
					return false;
				}
				if (!pathnameDevice().equalp(p.pathnameDevice())) {
					return false;
				}
				if (!pathnameDirectory().equalp(p.pathnameDirectory())) {
					return false;
				}
				if (!pathnameName().equalp(p.pathnameName())) {
					return false;
				}
				if (!pathnameType().equalp(p.pathnameType())) {
					return false;
				}
				// Ignore version component.
				//if (!pathnameVersion().equalp(p.pathnameVersion()))
				//    return false;
			} else {
				// Unix.
				if (!pathnameHost().equal(p.pathnameHost())) {
					return false;
				}
				if (!pathnameDevice().equal(p.pathnameDevice())) {
					return false;
				}
				if (!pathnameDirectory().equal(p.pathnameDirectory())) {
					return false;
				}
				if (!pathnameName().equal(p.pathnameName())) {
					return false;
				}
				if (!pathnameType().equal(p.pathnameType())) {
					return false;
				}
				// Ignore version component.
				//if (!pathnameVersion().equal(p.pathnameVersion()))
				//    return false;
			}
			return true;
		}
		return false;
	}

	static PathnameStruct toPathname(final String pathname) {
		return new PathnameStructImpl(pathname);
	}

	static PathnameStruct toPathname(final LispStruct host, final LispStruct device, final LispStruct directory,
	                                 final LispStruct name, final LispStruct type, final LispStruct version) {
		return new PathnameStructImpl(host, device, directory, name, type, version);
	}

	static PathnameStruct fromDesignator(final LispStruct struct) {
		if (struct instanceof PathnameStruct) {
			return (PathnameStruct) struct;
		}
		if (struct instanceof StringStruct) {
			final StringStruct namestringStruct = (StringStruct) struct;
			final String namestring = namestringStruct.toJavaString();
			return toPathname(namestring);
		}
		if (struct instanceof FileStreamStruct) {
			final FileStreamStruct fileStream = (FileStreamStruct) struct;
			return fileStream.toPathname();
		}
		throw new TypeErrorException("Type cannot be converted to PATHNAME.");
	}
}
