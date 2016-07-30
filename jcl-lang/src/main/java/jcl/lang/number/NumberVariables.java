/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.VariableStruct;

public interface NumberVariables {

	VariableStruct<RandomStateStruct> RANDOM_STATE = VariableStruct.valueOf("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, RandomStateStruct.valueOf());
}
