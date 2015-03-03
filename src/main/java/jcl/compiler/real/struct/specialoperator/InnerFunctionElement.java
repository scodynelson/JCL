/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.specialoperator.InnerFunctionElement.InnerFunctionVar;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.List;

public abstract class InnerFunctionElement<E extends Environment, V extends InnerFunctionVar> implements LispStruct {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<V> vars;

	private final List<LispStruct> forms;

	private final E lexicalEnvironment;

	InnerFunctionElement(final List<V> vars, final List<LispStruct> forms, final E lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<V> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public E getLexicalEnvironment() {
		return lexicalEnvironment;
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

	public static class InnerFunctionVar implements Serializable {

		private static final long serialVersionUID = 891453745075246590L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		InnerFunctionVar(final SymbolStruct<?> var, final LispStruct initForm) {
			this.var = var;
			this.initForm = initForm;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getInitForm() {
			return initForm;
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
