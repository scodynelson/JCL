/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ProgvStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 6286708668973616872L;

	private final List<ProgvVar> vars;

	private final PrognStruct forms;

	private final List<Binding<?>> previousBindings;

	private final Environment environment;

	public ProgvStruct(final List<ProgvVar> vars, final PrognStruct forms, final List<Binding<?>> previousBindings,
	                   final Environment environment) {
		this.vars = vars;
		this.forms = forms;
		this.previousBindings = previousBindings;
		this.environment = environment;
	}

	public List<ProgvVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public List<Binding<?>> getPreviousBindings() {
		return previousBindings;
	}

	public Environment getEnvironment() {
		return environment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(previousBindings)
		                            .append(environment)
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
		final ProgvStruct rhs = (ProgvStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(previousBindings, rhs.previousBindings)
		                          .append(environment, rhs.environment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(previousBindings)
		                                                                .append(environment)
		                                                                .toString();
	}

	public static class ProgvVar implements Serializable {

		private static final long serialVersionUID = -5131005121770228469L;

		private final SymbolStruct<?> var;

		private final LispStruct val;

		public ProgvVar(final SymbolStruct<?> var, final LispStruct val) {
			this.var = var;
			this.val = val;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getVal() {
			return val;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(var)
			                            .append(val)
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
			final ProgvVar rhs = (ProgvVar) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(val, rhs.val)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .append(val)
			                                                                .toString();
		}
	}
}
