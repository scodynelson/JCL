/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.Binding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ProgvStruct implements LispStruct {

	private static final long serialVersionUID = 6286708668973616872L;

	private final List<ProgvVar> vars;

	private final List<LispStruct> forms;

	private final List<Binding<?>> previousBindings;

	private final Environment environment;

	public ProgvStruct(final List<ProgvVar> vars, final List<LispStruct> forms, final List<Binding<?>> previousBindings,
	                   final Environment environment) {
		this.vars = vars;
		this.forms = forms;
		this.previousBindings = previousBindings;
		this.environment = environment;
	}

	public List<ProgvVar> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public List<Binding<?>> getPreviousBindings() {
		return previousBindings;
	}

	public Environment getEnvironment() {
		return environment;
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
}
