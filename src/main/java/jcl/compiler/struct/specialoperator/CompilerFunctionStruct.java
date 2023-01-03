/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.environment.Environment;
import jcl.lang.LispStruct;

public interface CompilerFunctionStruct extends LispStruct {

	LispStruct eval(final Environment environment);
}
