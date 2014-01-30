package jcl.structs.pathnames;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.pathnames.LogicalPathname;

public class LogicalPathnameStruct implements LispStruct {

	/*
	(defstruct (logical-pathname
	    (:conc-name %logical-pathname-)
	    (:print-function %print-logical-pathname)
	    (:include pathname)
	    (:constructor %make-logical-pathname
			  (host device directory name type version))
	    (:make-load-form-fun :just-dump-it-normally)))
	 */

	@Override
	public LispType getType() {
		return LogicalPathname.INSTANCE;
	}
}
