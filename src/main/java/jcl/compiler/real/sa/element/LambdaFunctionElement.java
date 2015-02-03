/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

public class LambdaFunctionElement implements FunctionElement {

	private static final long serialVersionUID = 1418688382783925560L;

	private final LambdaElement lambdaElement;

	public LambdaFunctionElement(final LambdaElement lambdaElement) {
		this.lambdaElement = lambdaElement;
	}

	public LambdaElement getLambdaElement() {
		return lambdaElement;
	}
}
