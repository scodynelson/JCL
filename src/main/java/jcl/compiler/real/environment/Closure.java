package jcl.compiler.real.environment;

import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

import java.util.List;

public class Closure {

	private int depth;
	private Scope scope;
	private List<ClosureBinding> bindings;

	public Closure(final int depth, final Scope scope, final List<ClosureBinding> bindings) {
		this.depth = depth;
		this.scope = scope;
		this.bindings = bindings;
	}

	public int getDepth() {
		return depth;
	}

	public Scope getScope() {
		return scope;
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

	public void setScope(final Scope scope) {
		this.scope = scope;
	}

	public void setBindings(final List<ClosureBinding> bindings) {
		this.bindings = bindings;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}
}
