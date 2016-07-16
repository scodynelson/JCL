/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.lang.number.IntegerStruct;

public interface SymbolVariables {

	VariableStruct<IntegerStruct> GENSYM_COUNTER = new VariableStruct<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
}
