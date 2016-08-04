/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.internal.VariableStruct;

public interface SymbolVariables {

	VariableStruct<IntegerStruct> GENSYM_COUNTER = VariableStruct.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
}
