/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class LocallyStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 3549306656634788482L;

	private final PrognStruct forms;

	private final Environment locallyEnvironment;

	public LocallyStruct(final PrognStruct forms, final Environment locallyEnvironment) {
		this.forms = forms;
		this.locallyEnvironment = locallyEnvironment;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLocallyEnvironment() {
		return locallyEnvironment;
	}
}
