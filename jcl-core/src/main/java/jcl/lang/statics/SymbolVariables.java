/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.IntegerStruct;
import jcl.lang.internal.VariableStructImpl;

public interface SymbolVariables {

	VariableStructImpl<IntegerStruct> GENSYM_COUNTER = VariableStructImpl.valueOf("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
}
