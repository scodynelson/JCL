/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<O extends LispStruct, I extends LispStruct> extends FunctionStruct {

	private static final long serialVersionUID = 4976821260573562104L;

	public abstract O expand(I form, Environment environment);
}
