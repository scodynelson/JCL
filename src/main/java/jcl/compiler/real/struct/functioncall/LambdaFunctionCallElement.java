/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.functioncall;

import jcl.LispStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaElement;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class LambdaFunctionCallElement implements LispStruct {

	private static final long serialVersionUID = -6330612245404973713L;

	private final LambdaElement lambdaElement;

	private final List<LispStruct> arguments;

	public LambdaFunctionCallElement(final LambdaElement lambdaElement, final List<LispStruct> arguments) {
		this.lambdaElement = lambdaElement;
		this.arguments = arguments;
	}

	public LambdaElement getLambdaElement() {
		return lambdaElement;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
