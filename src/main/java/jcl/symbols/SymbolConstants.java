/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols;

import jcl.packages.GlobalPackageStruct;

public interface SymbolConstants {

	Constant<NILStruct> NIL = new Constant<>("NIL", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	Constant<TStruct> T = new Constant<>("T", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
}
