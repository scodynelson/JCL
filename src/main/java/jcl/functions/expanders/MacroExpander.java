/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;

public abstract class MacroExpander<O extends LispStruct, I extends LispStruct> extends FunctionStruct {

	private static final long serialVersionUID = 4976821260573562104L;

	protected MacroExpander() {
		this(null);
	}

	protected MacroExpander(final Closure closure) {
		super(closure);
	}

	protected MacroExpander(final String documentation, final Closure closure) {
		super(documentation, closure);
	}

	public abstract O expand(I form, Environment environment);
}
