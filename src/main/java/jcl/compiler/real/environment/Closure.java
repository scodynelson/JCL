package jcl.compiler.real.environment;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class Closure {

	private final List<ClosureBinding> bindings = new ArrayList<>();
	private final int depth;

	public Closure(final int depth) {
		this.depth = depth;
	}

	public List<ClosureBinding> getBindings() {
		return bindings;
	}

	public ClosureBinding getBinding(final SymbolStruct<?> symbolStruct) {
		for (final ClosureBinding closureBinding : bindings) {
			if (closureBinding.getSymbolStruct().equals(symbolStruct)) {
				return closureBinding;
			}
		}
		return null;
	}

	public int getDepth() {
		return depth;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
