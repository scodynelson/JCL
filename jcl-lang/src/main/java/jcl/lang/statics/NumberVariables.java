/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.RandomStateStruct;
import jcl.lang.internal.number.RandomStateStructImpl;
import jcl.lang.VariableStruct;

public interface NumberVariables {

	VariableStruct<RandomStateStruct> RANDOM_STATE = VariableStruct.valueOf("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, RandomStateStructImpl.valueOf());
}
