/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols;

import jcl.packages.GlobalPackageStruct;

public interface SymbolConstants {

	ConstantStruct<NILStruct> NIL = new ConstantStruct<>("NIL", GlobalPackageStruct.COMMON_LISP, NILStruct.INSTANCE);
	ConstantStruct<TStruct> T = new ConstantStruct<>("T", GlobalPackageStruct.COMMON_LISP, TStruct.INSTANCE);
}
