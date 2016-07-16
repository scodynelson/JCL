/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.VariableStruct;

public interface NumberVariables {

	VariableStruct<RandomStateStruct> RANDOM_STATE = new VariableStruct<>("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, new RandomStateStruct());
}
