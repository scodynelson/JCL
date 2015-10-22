/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class ProgvStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 6286708668973616872L;

	private final LispStruct vars;

	private final LispStruct vals;

	private final PrognStruct forms;

	private final ProgvEnvironment progvEnvironment;

	public ProgvStruct(final LispStruct vars, final LispStruct vals, final PrognStruct forms, final ProgvEnvironment progvEnvironment) {
		this.vars = vars;
		this.vals = vals;
		this.forms = forms;
		this.progvEnvironment = progvEnvironment;
	}

	public LispStruct getVars() {
		return vars;
	}

	public LispStruct getVals() {
		return vals;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public ProgvEnvironment getProgvEnvironment() {
		return progvEnvironment;
	}
}
