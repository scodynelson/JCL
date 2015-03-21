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
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
		}
	}
}
