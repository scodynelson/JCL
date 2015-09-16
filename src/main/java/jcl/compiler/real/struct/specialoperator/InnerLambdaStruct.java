/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.compiler.real.environment.InnerLambdaEnvironment;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class InnerLambdaStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<InnerLambdaVar> vars;

	private final PrognStruct forms;

	private final InnerLambdaEnvironment lexicalEnvironment;

	public InnerLambdaStruct(final List<InnerLambdaVar> vars, final PrognStruct forms, final InnerLambdaEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<InnerLambdaVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public InnerLambdaEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(lexicalEnvironment)
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
		final InnerLambdaStruct rhs = (InnerLambdaStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(lexicalEnvironment, rhs.lexicalEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(lexicalEnvironment)
		                                                                .toString();
	}

	public static class InnerLambdaVar implements Serializable {

		private static final long serialVersionUID = 891453745075246590L;

		private final SymbolStruct<?> var;

		private final CompilerFunctionStruct initForm;

		private final boolean isSpecial;

		public InnerLambdaVar(final SymbolStruct<?> var, final CompilerFunctionStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public CompilerFunctionStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(var)
			                            .append(initForm)
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
			final InnerLambdaVar rhs = (InnerLambdaVar) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(initForm, rhs.initForm)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .toString();
		}
	}
}
