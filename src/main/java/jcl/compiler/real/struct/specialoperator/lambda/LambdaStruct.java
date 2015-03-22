/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.lambda;

import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -3234593852889478530L;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final PrognStruct forms;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaStruct(final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString, final PrognStruct forms,
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

	public PrognStruct getForms() {
		return forms;
	}

	public LambdaEnvironment getLambdaEnvironment() {
		return lambdaEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(lambdaListBindings)
		                            .append(docString)
		                            .append(forms)
		                            .append(lambdaEnvironment)
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
		final LambdaStruct rhs = (LambdaStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(lambdaListBindings, rhs.lambdaListBindings)
		                          .append(docString, rhs.docString)
		                          .append(forms, rhs.forms)
		                          .append(lambdaEnvironment, rhs.lambdaEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(lambdaListBindings)
		                                                                .append(docString)
		                                                                .append(forms)
		                                                                .append(lambdaEnvironment)
		                                                                .toString();
	}
}
