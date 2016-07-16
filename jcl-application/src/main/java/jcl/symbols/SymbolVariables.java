/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;

public interface SymbolVariables {

	VariableStruct<IntegerStruct> GENSYM_COUNTER = new VariableStruct<>("*GENSYM-COUNTER*", GlobalPackageStruct.COMMON_LISP, IntegerStruct.ZERO);
}
