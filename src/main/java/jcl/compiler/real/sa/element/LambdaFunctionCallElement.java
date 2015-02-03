/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class LambdaFunctionCallElement implements Element {

	private static final long serialVersionUID = -6330612245404973713L;

	private final LambdaElement lambdaElement;
	private final List<LispStruct> arguments;

	public LambdaFunctionCallElement(final LambdaElement lambdaElement, final List<LispStruct> arguments) {
		this.lambdaElement = lambdaElement;
		this.arguments = arguments;
	}
}
