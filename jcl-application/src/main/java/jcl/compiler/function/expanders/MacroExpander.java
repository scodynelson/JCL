/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function.expanders;

import jcl.compiler.environment.Environment;
import jcl.lang.LispStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.MacroExpanderInter;

public abstract class MacroExpander<O extends LispStruct, I extends LispStruct> extends FunctionStruct implements MacroExpanderInter {

	protected MacroExpander(final String documentation) {
		super(documentation);
	}

	public abstract O expand(I form, Environment environment);
}
