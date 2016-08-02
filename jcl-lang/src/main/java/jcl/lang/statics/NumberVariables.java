/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.number.RandomStateStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.VariableStruct;

public interface NumberVariables {

	VariableStruct<RandomStateStruct> RANDOM_STATE = VariableStruct.valueOf("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, RandomStateStruct.valueOf());
}
