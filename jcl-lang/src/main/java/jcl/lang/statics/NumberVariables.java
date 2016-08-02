/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.number.RandomStateStructImpl;
import jcl.lang.VariableStruct;

public interface NumberVariables {

	VariableStruct<RandomStateStructImpl> RANDOM_STATE = VariableStruct.valueOf("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, RandomStateStructImpl.valueOf());
}
