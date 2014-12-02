package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Closure {

	private final List<ClosureBinding> bindings = new ArrayList<>();
	private final int depth;

	public Closure(final int depth) {
		this.depth = depth;
	}

	public List<ClosureBinding> getBindings() {
		return bindings;
	}

	public Optional<ClosureBinding> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public void addBinding(final SymbolStruct<?> newVariable) {
		final ClosureBinding closureBinding = new ClosureBinding(newVariable, bindings.size(), 1);
		bindings.add(closureBinding);
	}

	public int getDepth() {
		return depth;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
