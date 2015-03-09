/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import java.io.Serializable;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ClosureBinding implements Serializable {

	private static final long serialVersionUID = 3814280129166840828L;

	private final SymbolStruct<?> symbolStruct;

	private final int position;

	private int references;

	public ClosureBinding(final SymbolStruct<?> symbolStruct, final int position, final int references) {
		this.symbolStruct = symbolStruct;
		this.position = position;
		this.references = references;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public int getPosition() {
		return position;
	}

	public int getReferences() {
		return references;
	}

	public void incrementReferences() {
		references++;
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
