/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages;

import jcl.symbols.Variable;

public interface PackageVariables {

	Variable<PackageStruct> PACKAGE = new Variable<>("*PACKAGE*", GlobalPackageStruct.COMMON_LISP, GlobalPackageStruct.COMMON_LISP_USER);
}
