/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.LabelsEnvironment;
import jcl.compiler.real.struct.specialoperator.LabelsElement.LabelsVar;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class LabelsElement extends InnerFunctionElement<LabelsEnvironment, LabelsVar> {

	private static final long serialVersionUID = -2347494500321073144L;

	public LabelsElement(final List<LabelsVar> vars, final List<LispStruct> forms, final LabelsEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
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

	public static class LabelsVar extends InnerFunctionElement.InnerFunctionVar {

		private static final long serialVersionUID = 2989214415282349607L;

		public LabelsVar(final SymbolStruct<?> var, final LispStruct initForm) {
			super(var, initForm);
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
