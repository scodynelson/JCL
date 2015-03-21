/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.InnerFunctionStruct.InnerFunctionVar;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class InnerFunctionStruct<E extends Environment, V extends InnerFunctionVar> extends SpecialOperatorStruct {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<V> vars;

	private final PrognStruct forms;

	private final E lexicalEnvironment;

	InnerFunctionStruct(final List<V> vars, final PrognStruct forms, final E lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<V> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public E getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class InnerFunctionVar implements Serializable {

		private static final long serialVersionUID = 891453745075246590L;

		private final SymbolStruct<?> var;

		private final CompilerFunctionStruct initForm;

		InnerFunctionVar(final SymbolStruct<?> var, final CompilerFunctionStruct initForm) {
			this.var = var;
			this.initForm = initForm;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public CompilerFunctionStruct getInitForm() {
			return initForm;
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
