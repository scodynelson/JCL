/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Variable;

public interface PathnameVariables {

	Variable<?> DEFAULT_PATHNAME_DEFAULTS = new Variable<>("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, null);
}
