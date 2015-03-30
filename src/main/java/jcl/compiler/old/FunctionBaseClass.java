package jcl.compiler.old;

import java.util.Stack;

import jcl.compiler.old.symbol.Closure;

public abstract class FunctionBaseClass {

	protected final Stack<Closure> closures = new Stack<>();

	public FunctionBaseClass(final Closure closure) {
		closures.push(closure);
	}

	public Closure getClosure() {
		if (closures.empty()) {
			return null;
		}
		return closures.peek();
	}

	public Closure addClosure(final Closure closure) {
		return closures.push(closure);
	}

	public Closure popClosure() {
		return closures.pop();
	}

	public Stack<Closure> getClosureStack() {
		return closures;
	}
}
