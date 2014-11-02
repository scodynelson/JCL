package jcl.compiler.real.environment;

import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class Closure {

	private int depth;
	private List<ClosureBinding> bindings;

	public Closure(final int depth, final List<ClosureBinding> bindings) {
		this.depth = depth;
		this.bindings = bindings;
	}

	public int getDepth() {
		return depth;
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

	public void setDepth(final int depth) {
		this.depth = depth;
	}

	public void setBindings(final List<ClosureBinding> bindings) {
		this.bindings = bindings;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
