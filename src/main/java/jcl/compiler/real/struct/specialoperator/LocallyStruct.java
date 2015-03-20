/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class LocallyStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 3549306656634788482L;

	private final PrognStruct forms;

	private final LocallyEnvironment locallyEnvironment;

	public LocallyStruct(final PrognStruct forms, final LocallyEnvironment locallyEnvironment) {
		this.forms = forms;
		this.locallyEnvironment = locallyEnvironment;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public LocallyEnvironment getLocallyEnvironment() {
		return locallyEnvironment;
	}
}
