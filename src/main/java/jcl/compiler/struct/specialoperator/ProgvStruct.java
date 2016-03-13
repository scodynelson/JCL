/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.ProgvEnvironment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class ProgvStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct vars;

	private final LispStruct vals;

	private final PrognStruct forms;

	private final ProgvEnvironment progvEnvironment;

	public ProgvStruct(final LispStruct vars, final LispStruct vals, final List<LispStruct> forms, final ProgvEnvironment progvEnvironment) {
		this.vars = vars;
		this.vals = vals;
		this.forms = new PrognStruct(forms);
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
