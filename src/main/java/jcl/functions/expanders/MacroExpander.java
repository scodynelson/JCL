/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<O extends LispStruct, I extends LispStruct> extends FunctionStruct {

	protected MacroExpander() {
	}

	protected MacroExpander(final String documentation) {
		super(documentation);
	}

	public abstract O expand(I form, Environment environment);
}
