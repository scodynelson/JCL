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

	static LogicalPathnameStruct toLogicalPathname(final LispStruct struct) {
		if (struct instanceof LogicalPathnameStruct) {
			return (LogicalPathnameStruct) struct;
		}
		if (struct instanceof StringStruct) {
			final StringStruct namestringStruct = (StringStruct) struct;
			final String namestring = namestringStruct.toJavaString();
			return toLogicalPathname(namestring);
		}
		if (struct instanceof StreamStruct) {
			final PathnameStruct pathnameStruct = PathnameStruct.toPathname(struct);
			if (pathnameStruct instanceof LogicalPathnameStruct) {
				return (LogicalPathnameStruct) pathnameStruct;
			}
		}
		throw new TypeErrorException("Type cannot be converted to LOGICAL-PATHNAME.");
	}

}
