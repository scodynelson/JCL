/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.LetStarEnvironment;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LetStarStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -3186671381163635893L;

	private final List<LetStarVar> vars;

	private final PrognStruct forms;

	private final LetStarEnvironment letStarEnvironment;

	public LetStarStruct(final List<LetStarVar> vars, final PrognStruct forms, final LetStarEnvironment letStarEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.letStarEnvironment = letStarEnvironment;
	}

	public List<LetStarVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public LetStarEnvironment getLetStarEnvironment() {
		return letStarEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(letStarEnvironment)
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
		final LetStarStruct rhs = (LetStarStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(letStarEnvironment, rhs.letStarEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(letStarEnvironment)
		                                                                .toString();
	}

	public static class LetStarVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetStarVar(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial) {
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
			final LetStarVar rhs = (LetStarVar) obj;
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
