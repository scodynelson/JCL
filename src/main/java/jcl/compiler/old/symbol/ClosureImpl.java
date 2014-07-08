package jcl.compiler.old.symbol;

public class ClosureImpl implements Closure {

	private Closure parent;
	private Object[] boundValues;

	/**
	 * Creates a new instance of ClosureImpl
	 */
	public ClosureImpl(Closure parent, int size) {
		this.parent = parent;
		if ((parent != null) && (!(parent instanceof Closure))) {
			throw new IllegalArgumentException("parent is not a Closure (constructor): " + parent);
		}
		boundValues = new Object[size];
	}

	public Object getBindingAt(int index, int nestingLevel) {
		if (nestingLevel == 0) {
			return boundValues[index];
		} else {
			if ((parent != null) && (!(parent instanceof Closure))) {
				throw new IllegalArgumentException("parent is not a Closure (getbinding): " + parent);
			}
			return parent.getBindingAt(index, nestingLevel - 1);
		}
	}

	public Closure getParent() {
		return parent;
	}

	public void setBindingAt(int index, int nestingLevel, Object value) {
		if (nestingLevel == 0) {
			boundValues[index] = value;
		} else {
			if ((parent != null) && (!(parent instanceof Closure))) {
				throw new IllegalArgumentException("parent is not a Closure (setbinding): " + parent);
			}
			parent.setBindingAt(index, nestingLevel - 1, value);
		}
	}

}
