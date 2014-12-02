package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class SymbolTable {

	private final List<SymbolBinding> bindings = new ArrayList<>();

	public List<SymbolBinding> getBindings() {
		return bindings;
	}

	public SymbolBinding getBinding(final SymbolStruct<?> symbolStruct) {
		for (final SymbolBinding binding : bindings) {
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
