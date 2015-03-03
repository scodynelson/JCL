/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolMacroElement implements LispStruct {

	private static final long serialVersionUID = -1618487345643376983L;

	private final SymbolStruct<?> symbolStruct;

	private final LispStruct form;

	public SymbolMacroElement(final SymbolStruct<?> symbolStruct, final LispStruct form) {
		this.symbolStruct = symbolStruct;
		this.form = form;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public LispStruct getForm() {
		return form;
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
