package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class SymbolBinding extends Binding {

	private Environment binding;

	public SymbolBinding(final SymbolStruct symbolStruct, final Allocation allocation, final Scope scope,
						 final LispType type, final Environment binding, final boolean isRequired) {
		super(symbolStruct, allocation, scope, type, isRequired);
		this.binding = binding;
	}

	public Environment getBinding() {
		return binding;
	}

	public void setBinding(final Environment binding) {
		this.binding = binding;
	}
}
