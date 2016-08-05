/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.pathname.PathnameStructImpl;
import jcl.lang.internal.VariableStructImpl;

public interface PathnameVariables {

	VariableStructImpl<PathnameStructImpl> DEFAULT_PATHNAME_DEFAULTS = VariableStructImpl.valueOf("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, PathnameStructImpl.valueOf(""));
}
