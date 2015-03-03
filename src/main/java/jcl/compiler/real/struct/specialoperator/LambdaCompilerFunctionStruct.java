/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaCompilerFunctionStruct implements CompilerFunctionStruct {

	private static final long serialVersionUID = 1418688382783925560L;

	private final LambdaStruct lambdaStruct;

	public LambdaCompilerFunctionStruct(final LambdaStruct lambdaStruct) {
		this.lambdaStruct = lambdaStruct;
	}

	public LambdaStruct getLambdaStruct() {
		return lambdaStruct;
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
