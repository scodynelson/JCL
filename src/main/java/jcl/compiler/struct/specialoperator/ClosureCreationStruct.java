/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public abstract class ClosureCreationStruct<V> extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 9216281797404250223L;

	private final List<V> vars;

	private final PrognStruct forms;

	private final Environment environment;

	protected ClosureCreationStruct(final List<V> vars, final PrognStruct forms, final Environment environment) {
		this.vars = vars;
		this.forms = forms;
		this.environment = environment;
	}

	public List<V> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getEnvironment() {
		return environment;
	}
}
