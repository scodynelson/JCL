/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LetEnvironment;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.List;

public class LetStruct implements LispStruct {

	private static final long serialVersionUID = -3186671381163635893L;

	private final List<LetVar> vars;

	private final List<LispStruct> forms;

	private final LetEnvironment letEnvironment;

	public LetStruct(final List<LetVar> vars, final List<LispStruct> forms, final LetEnvironment letEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.letEnvironment = letEnvironment;
	}

	public List<LetVar> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public LetEnvironment getLetEnvironment() {
		return letEnvironment;
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

	public static class LetVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		public LetVar(final SymbolStruct<?> var, final LispStruct initForm) {
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
