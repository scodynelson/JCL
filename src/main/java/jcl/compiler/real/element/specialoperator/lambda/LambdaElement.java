/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.lambda;

import jcl.arrays.StringStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;

import java.util.List;

public class LambdaElement implements Element {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final List<Element> forms;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaElement(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final List<Element> forms,
	                     final LambdaEnvironment lambdaEnvironment) {
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}
}
