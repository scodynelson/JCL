/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolMacroletStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 2362123144166759037L;

	private final List<SymbolMacroletElementVar> vars;

	private final PrognStruct forms;

	private final SymbolMacroletEnvironment symbolMacroletEnvironment;

	public SymbolMacroletStruct(final List<SymbolMacroletElementVar> vars, final PrognStruct forms, final SymbolMacroletEnvironment symbolMacroletEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.symbolMacroletEnvironment = symbolMacroletEnvironment;
	}

	public List<SymbolMacroletElementVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public SymbolMacroletEnvironment getSymbolMacroletEnvironment() {
		return symbolMacroletEnvironment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(vars)
		                            .append(forms)
		                            .append(symbolMacroletEnvironment)
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
		final SymbolMacroletStruct rhs = (SymbolMacroletStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(vars, rhs.vars)
		                          .append(forms, rhs.forms)
		                          .append(symbolMacroletEnvironment, rhs.symbolMacroletEnvironment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(vars)
		                                                                .append(forms)
		                                                                .append(symbolMacroletEnvironment)
		                                                                .toString();
	}

	public static class SymbolMacroletElementVar implements Serializable {

		private static final long serialVersionUID = -601687250765470819L;

		private final SymbolStruct<?> var;

		private final LispStruct expansion;

		public SymbolMacroletElementVar(final SymbolStruct<?> var, final LispStruct expansion) {
			this.var = var;
			this.expansion = expansion;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getExpansion() {
			return expansion;
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().append(var)
			                            .append(expansion)
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
			final SymbolMacroletElementVar rhs = (SymbolMacroletElementVar) obj;
			return new EqualsBuilder().append(var, rhs.var)
			                          .append(expansion, rhs.expansion)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
			                                                                .append(expansion)
			                                                                .toString();
		}
	}
}
