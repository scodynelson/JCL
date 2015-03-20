/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import jcl.LispStruct;

public interface IntermediateCodeGenerator {

	Object funcall(LispStruct lispFunc);
}
