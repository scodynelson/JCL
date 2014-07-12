package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public abstract class Binding {

	private SymbolStruct symbolStruct;
	private Allocation allocation;
	private Scope scope;
	private LispType type;

	private boolean isRequired;

	protected Binding(final SymbolStruct symbolStruct, final Allocation allocation, final Scope scope,
					  final LispType type, final boolean isRequired) {
		this.symbolStruct = symbolStruct;

		this.allocation = allocation;
		this.scope = scope;
		this.type = type;
		this.isRequired = isRequired;
	}

	public SymbolStruct getSymbolStruct() {
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

	public boolean isRequired() {
		return isRequired;
	}

	public void setSymbolStruct(final SymbolStruct symbolStruct) {
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

	public void setRequired(final boolean isRequired) {
		this.isRequired = isRequired;
	}
}
