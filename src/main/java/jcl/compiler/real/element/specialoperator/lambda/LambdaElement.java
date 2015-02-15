/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.lambda;

import jcl.arrays.StringStruct;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;

public class LambdaElement implements Element {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final Element body;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaElement(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final Element body,
	                     final LambdaEnvironment lambdaEnvironment) {
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.body = body;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public Element getBody() {
		return body;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}
}
