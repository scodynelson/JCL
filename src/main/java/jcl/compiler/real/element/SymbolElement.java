/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolElement<T extends LispStruct> implements Element {

	private static final long serialVersionUID = 6848146265489259786L;

	private final SymbolStruct<T> symbolStruct;

	public SymbolElement(final SymbolStruct<T> symbolStruct) {
		this.symbolStruct = symbolStruct;
	}

	public SymbolStruct<T> getSymbolStruct() {
		return symbolStruct;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
