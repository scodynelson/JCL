/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.pathname.PathnameStruct;
import jcl.lang.internal.VariableStruct;

public interface PathnameVariables {

	VariableStruct<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS = VariableStruct.valueOf("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, PathnameStruct.valueOf(""));
}
