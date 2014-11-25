/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Variable;

public interface NumberVariables {

    Variable<RandomStateStruct> RANDOM_STATE = new Variable<>("*RANDOM-STATE*", GlobalPackageStruct.COMMON_LISP, new RandomStateStruct());

}
