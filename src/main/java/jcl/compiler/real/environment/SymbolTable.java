package jcl.compiler.real.environment;

import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class SymbolTable {

	private final List<Binding> bindings = new ArrayList<>();

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
