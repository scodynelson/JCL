/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class LocallyStruct extends CompilerSpecialOperatorStruct {

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
