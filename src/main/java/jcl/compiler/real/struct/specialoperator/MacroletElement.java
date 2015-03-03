/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.environment.MacroletEnvironment;
import jcl.compiler.real.struct.specialoperator.MacroletElement.MacroletVar;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class MacroletElement extends InnerFunctionElement<MacroletEnvironment, MacroletVar> {

	private static final long serialVersionUID = -6865772116422991356L;

	public MacroletElement(final List<MacroletVar> vars, final List<LispStruct> forms, final MacroletEnvironment lexicalEnvironment) {
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

	public static class MacroletVar extends InnerFunctionElement.InnerFunctionVar {

		private static final long serialVersionUID = -169311089356148669L;

		public MacroletVar(final SymbolStruct<?> var, final LispStruct initForm) {
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
