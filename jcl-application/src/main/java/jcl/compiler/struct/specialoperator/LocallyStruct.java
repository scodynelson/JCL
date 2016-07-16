/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class LocallyStruct extends CompilerSpecialOperatorStruct {

	private final PrognStruct forms;

	private final Environment locallyEnvironment;

	public LocallyStruct(final List<LispStruct> forms, final Environment locallyEnvironment) {
		this.forms = new PrognStruct(forms);
		this.locallyEnvironment = locallyEnvironment;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getLocallyEnvironment() {
		return locallyEnvironment;
	}
}
