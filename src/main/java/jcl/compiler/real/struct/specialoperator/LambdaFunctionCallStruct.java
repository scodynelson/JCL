/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaFunctionCallStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -6330612245404973713L;

	private final LambdaStruct lambdaStruct;

	private final List<LispStruct> arguments;

	public LambdaFunctionCallStruct(final LambdaStruct lambdaStruct, final List<LispStruct> arguments) {
		this.lambdaStruct = lambdaStruct;
		this.arguments = arguments;
	}

	public LambdaStruct getLambdaStruct() {
		return lambdaStruct;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(lambdaStruct)
		                            .append(arguments)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final LambdaFunctionCallStruct rhs = (LambdaFunctionCallStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(lambdaStruct, rhs.lambdaStruct)
		                          .append(arguments, rhs.arguments)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(lambdaStruct)
		                                                                .append(arguments)
		                                                                .toString();
	}
}
