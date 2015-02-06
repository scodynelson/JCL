/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.allocation.Allocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;

public abstract class Binding<A extends Allocation> implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolStruct<?> symbolStruct;

	private final A allocation;

	private final Scope scope;

	private final LispType type;

	protected Binding(final SymbolStruct<?> symbolStruct, final A allocation, final Scope scope,
	                  final LispType type) {
		this.symbolStruct = symbolStruct;
		this.allocation = allocation;
		this.scope = scope;
		this.type = type;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public A getAllocation() {
		return allocation;
	}

	public Scope getScope() {
		return scope;
	}

	public LispType getType() {
		return type;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
