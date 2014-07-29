package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;

import java.util.List;

public class SymbolTable {

	private List<Binding> bindings;

	public SymbolTable(final List<Binding> bindings) {
		this.bindings = bindings;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public Binding getBinding(final SymbolStruct<?> symbolStruct) {
		for (final Binding binding : bindings) {
			if (binding.getSymbolStruct().equals(symbolStruct)) {
				return binding;
			}
		}
		return null;
	}

	public void setBindings(final List<Binding> bindings) {
		this.bindings = bindings;
	}
}
