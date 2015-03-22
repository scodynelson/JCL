/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages;

import jcl.symbols.VariableStruct;

public interface PackageVariables {

	VariableStruct<PackageStruct> PACKAGE = new VariableStruct<>("*PACKAGE*", GlobalPackageStruct.COMMON_LISP, GlobalPackageStruct.COMMON_LISP_USER);
}
