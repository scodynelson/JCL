/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.List;

public class SymbolMacroletElement implements LispStruct {

	private static final long serialVersionUID = 2362123144166759037L;

	private final List<SymbolMacroletElementVar> vars;

	private final List<LispStruct> forms;

	private final SymbolMacroletEnvironment symbolMacroletEnvironment;

	public SymbolMacroletElement(final List<SymbolMacroletElementVar> vars, final List<LispStruct> forms, final SymbolMacroletEnvironment symbolMacroletEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.symbolMacroletEnvironment = symbolMacroletEnvironment;
	}

	public List<SymbolMacroletElementVar> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public SymbolMacroletEnvironment getSymbolMacroletEnvironment() {
		return symbolMacroletEnvironment;
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
}
