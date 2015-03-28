/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.nio.file.Paths;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface PathnameVariables {

	VariableStruct<PathnameStruct> DEFAULT_PATHNAME_DEFAULTS = new VariableStruct<>("*DEFAULT-PATHNAME-DEFAULTS*", GlobalPackageStruct.COMMON_LISP, PathnameStruct.buildPathname(Paths.get("")));
}
