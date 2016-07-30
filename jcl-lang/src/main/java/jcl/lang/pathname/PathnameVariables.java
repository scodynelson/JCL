/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.VariableStruct;

public interface PathnameVariables {

	VariableStruct<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS = new VariableStruct<>("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, PathnameStruct.valueOf(""));
}
