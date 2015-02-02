/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.lambdalist.OrdinaryLambdaListBindings;

import java.util.List;

public class LambdaElement implements Element {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final List<LispStruct> forms;

	private final Environment environment;

	public LambdaElement(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final List<LispStruct> forms, final Environment environment) {
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.environment = environment;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public Environment getEnvironment() {
		return environment;
	}
}
