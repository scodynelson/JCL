package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.LogicalPathnameStructImpl;

/**
 * The {@link LogicalPathnameStruct} is the object representation of a Lisp 'logical-pathname' type.
 */
public interface LogicalPathnameStruct extends PathnameStruct {

	static LogicalPathnameStruct toLogicalPathname(final String pathname) {
		return new LogicalPathnameStructImpl(pathname);
	}

	static LogicalPathnameStruct toLogicalPathname(final LispStruct host, final LispStruct directory,
	                                               final LispStruct name, final LispStruct type,
	                                               final LispStruct version) {
		return new LogicalPathnameStructImpl(host, directory, name, type, version);
	}

	static LogicalPathnameStruct fromDesignator(final LispStruct struct) {
		if (struct instanceof LogicalPathnameStruct) {
			return (LogicalPathnameStruct) struct;
		}
		if (struct instanceof final StringStruct namestringStruct) {
			final String namestring = namestringStruct.toJavaString();
			return toLogicalPathname(namestring);
		}
		if (struct instanceof final FileStreamStruct fileStream) {
			final PathnameStruct pathnameStruct = fileStream.toPathname();
			if (pathnameStruct instanceof LogicalPathnameStruct) {
				return (LogicalPathnameStruct) pathnameStruct;
			}
		}
		throw new TypeErrorException("Type cannot be converted to LOGICAL-PATHNAME.");
	}

}
