/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface PathnameVariables {

	VariableStruct<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS = new VariableStruct<>("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, null);
}
