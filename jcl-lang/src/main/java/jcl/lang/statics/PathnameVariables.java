/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.PathnameStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.pathname.PathnameStructImpl;

public interface PathnameVariables {

	VariableStructImpl<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS = VariableStructImpl.valueOf("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, PathnameStructImpl.valueOf(""));
}
