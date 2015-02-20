/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.compiler.real.element.specialoperator.lambda.LambdaElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class LambdaFunctionCallElement implements Element {

	private static final long serialVersionUID = -6330612245404973713L;

	private final LambdaElement lambdaElement;
	private final List<Element> arguments;

	public LambdaFunctionCallElement(final LambdaElement lambdaElement, final List<Element> arguments) {
		this.lambdaElement = lambdaElement;
		this.arguments = arguments;
	}

	public LambdaElement getLambdaElement() {
		return lambdaElement;
	}

	public List<Element> getArguments() {
		return arguments;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
