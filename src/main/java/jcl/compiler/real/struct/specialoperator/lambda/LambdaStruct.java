/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.lambda;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaStruct implements LispStruct {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final List<LispStruct> forms;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaStruct(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final List<LispStruct> forms,
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

	public List<LispStruct> getForms() {
		return forms;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
