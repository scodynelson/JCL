package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

public abstract class Binding {

	private SymbolStruct<?> symbolStruct;
	private Allocation allocation;
	private Scope scope;
	private LispType type;

	protected Binding(final SymbolStruct<?> symbolStruct, final Allocation allocation, final Scope scope, final LispType type) {
		this.symbolStruct = symbolStruct;
		this.allocation = allocation;
		this.scope = scope;
		this.type = type;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public Allocation getAllocation() {
		return allocation;
	}

	public Scope getScope() {
		return scope;
	}

	public LispType getType() {
		return type;
	}

	public void setSymbolStruct(final SymbolStruct<?> symbolStruct) {
		this.symbolStruct = symbolStruct;
	}

	public void setAllocation(final Allocation allocation) {
		this.allocation = allocation;
	}

	public void setScope(final Scope scope) {
		this.scope = scope;
	}

	public void setType(final LispType type) {
		this.type = type;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}
}
