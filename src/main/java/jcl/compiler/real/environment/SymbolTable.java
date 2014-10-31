package jcl.compiler.real.environment;

import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
