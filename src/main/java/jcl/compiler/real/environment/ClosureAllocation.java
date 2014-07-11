package jcl.compiler.real.environment;

public class ClosureAllocation implements Allocation {

	private Closure closure;

	public ClosureAllocation(final Closure closure) {
		this.closure = closure;
	}

	public Closure getClosure() {
		return closure;
	}

	public void setClosure(final Closure closure) {
		this.closure = closure;
	}
}
