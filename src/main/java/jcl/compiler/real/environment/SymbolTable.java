package jcl.compiler.real.environment;

import java.util.List;

public class SymbolTable {

	private List<Binding> bindings;

	public SymbolTable(final List<Binding> bindings) {
		this.bindings = bindings;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public void setBindings(final List<Binding> bindings) {
		this.bindings = bindings;
	}
}
