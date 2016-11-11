/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import jcl.lang.RandomStateStruct;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.internal.number.RandomStateStructImpl;

public interface NumberVariables {

	VariableStructImpl<RandomStateStruct> RANDOM_STATE = VariableStructImpl.valueOf("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, RandomStateStructImpl.valueOf());
}
