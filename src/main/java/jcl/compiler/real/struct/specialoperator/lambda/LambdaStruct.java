/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.lambda;

import java.util.List;

import jcl.arrays.StringStruct;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -3234593852889478530L;

	private final String fileName;

	private final OrdinaryLambdaListBindings lambdaListBindings;

	private final StringStruct docString;

	private final List<SetqStruct> initFormSetqs;

	private final PrognStruct forms;

	private final LambdaEnvironment lambdaEnvironment;

	public LambdaStruct(final String fileName, final OrdinaryLambdaListBindings lambdaListBindings, final StringStruct docString,
	                    final List<SetqStruct> initFormSetqs, final PrognStruct forms, final LambdaEnvironment lambdaEnvironment) {
		this.fileName = fileName;
		this.lambdaListBindings = lambdaListBindings;
		this.docString = docString;
		this.initFormSetqs = initFormSetqs;
		this.forms = forms;
		this.lambdaEnvironment = lambdaEnvironment;
	}

	public String getFileName() {
		return fileName;
	}

	public OrdinaryLambdaListBindings getLambdaListBindings() {
		return lambdaListBindings;
	}

	public StringStruct getDocString() {
		return docString;
	}

	public List<SetqStruct> getInitFormSetqs() {
		return initFormSetqs;
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
		                            .append(fileName)
		                            .append(lambdaListBindings)
		                            .append(docString)
		                            .append(initFormSetqs)
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
		                          .append(fileName, rhs.fileName)
		                          .append(lambdaListBindings, rhs.lambdaListBindings)
		                          .append(docString, rhs.docString)
		                          .append(initFormSetqs, rhs.initFormSetqs)
		                          .append(forms, rhs.forms)
		                          .append(lambdaEnvironment, rhs.lambdaEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(fileName)
		                                                                .append(lambdaListBindings)
		                                                                .append(docString)
		                                                                .append(initFormSetqs)
		                                                                .append(forms)
		                                                                .append(lambdaEnvironment)
		                                                                .toString();
	}
}
