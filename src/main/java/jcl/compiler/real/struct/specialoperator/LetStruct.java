/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LetStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -3186671381163635893L;

	private final List<LetVar> vars;

	private final PrognStruct forms;

	private final LetEnvironment letEnvironment;

	public LetStruct(final List<LetVar> vars, final PrognStruct forms, final LetEnvironment letEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.letEnvironment = letEnvironment;
	}

	public List<LetVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public LetEnvironment getLetEnvironment() {
		return letEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(letEnvironment)
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
		final LetStruct rhs = (LetStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(letEnvironment, rhs.letEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(letEnvironment)
		                                                                .toString();
	}

	public static class LetVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetVar(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getInitForm() {
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
			final LetVar rhs = (LetVar) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(initForm, rhs.initForm)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .append(initForm)
			                                                                .toString();
		}
	}
}
