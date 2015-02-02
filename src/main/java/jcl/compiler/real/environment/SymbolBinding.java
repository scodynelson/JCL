package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class SymbolBinding extends Binding {

	private final Environment<?> binding;

	public SymbolBinding(final SymbolStruct<?> symbolStruct, final Allocation allocation, final Scope scope,
	                     final LispType type, final Environment<?> binding) {
		super(symbolStruct, allocation, scope, type);
		this.binding = binding;
	}

	public Environment<?> getBinding() {
		return binding;
	}
}
