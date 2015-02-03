package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class SymbolTable {

	private final List<SymbolBinding> bindings = new ArrayList<>();

	public List<SymbolBinding> getBindings() {
		return bindings;
	}

	public boolean hasBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<SymbolBinding> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public void addBinding(final SymbolBinding symbolBinding) {
		bindings.add(symbolBinding);
	}

	public void addBinding(final SymbolStruct<?> newVariable, final Allocation allocation, final Scope scope, final LexicalEnvironment binding) {
		final SymbolBinding symbolBinding = new SymbolBinding(newVariable, allocation, scope, T.INSTANCE, binding);
		bindings.add(symbolBinding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
