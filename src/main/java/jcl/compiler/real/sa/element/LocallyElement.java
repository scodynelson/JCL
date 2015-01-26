/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;

import java.util.List;

public class LocallyElement implements Element {

	private static final long serialVersionUID = 3549306656634788482L;

	private final List<LispStruct> forms;

	private final Environment environment;

	public LocallyElement(final List<LispStruct> forms, final Environment environment) {
		this.forms = forms;
		this.environment = environment;
	}
}
