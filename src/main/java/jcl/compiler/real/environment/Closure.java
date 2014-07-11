package jcl.compiler.real.environment;

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

	public void setDepth(final int depth) {
		this.depth = depth;
	}

	public void setScope(final Scope scope) {
		this.scope = scope;
	}

	public void setBindings(final List<ClosureBinding> bindings) {
		this.bindings = bindings;
	}
}
