/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.VariableStruct;
import jcl.lang.number.IntegerStructImpl;

public interface SymbolVariables {

	VariableStruct<IntegerStructImpl> GENSYM_COUNTER = VariableStruct.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStructImpl.ZERO);
}
