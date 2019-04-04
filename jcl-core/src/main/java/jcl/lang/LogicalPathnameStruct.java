package jcl.lang;

import jcl.lang.internal.LogicalPathnameStructImpl;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;

/**
 * The {@link LogicalPathnameStruct} is the object representation of a Lisp 'logical-pathname' type.
 */
public interface LogicalPathnameStruct extends PathnameStruct {

	PathnameStruct translateLogicalPathname();

	static LogicalPathnameStruct toLogicalPathname(final String pathname) {
		return new LogicalPathnameStructImpl(pathname);
	}

	static LogicalPathnameStruct toLogicalPathname(final PathnameHost host,
	                                               final PathnameDirectory directory,
	                                               final PathnameName name,
	                                               final PathnameType type,
	                                               final PathnameVersion version) {
		return new LogicalPathnameStructImpl(host, directory, name, type, version);
	}

}
