/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaFunctionElement implements FunctionElement {

	private static final long serialVersionUID = 1418688382783925560L;

	private final LambdaElement lambdaElement;

	public LambdaFunctionElement(final LambdaElement lambdaElement) {
		this.lambdaElement = lambdaElement;
	}

	public LambdaElement getLambdaElement() {
		return lambdaElement;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
