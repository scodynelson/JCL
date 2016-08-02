/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.VariableStruct;
import jcl.lang.number.IntegerStruct;

public interface SymbolVariables {

	VariableStruct<IntegerStruct> GENSYM_COUNTER = VariableStruct.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
}
