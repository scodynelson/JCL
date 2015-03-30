package jcl.compiler.old.symbol;

public class ClosureImpl implements Closure {

	private Closure parent;

	private Object[] boundValues;

	public ClosureImpl(final Closure parent, final int size) {
		this.parent = parent;
		boundValues = new Object[size];
	}

	@Override
	public Closure getParent() {
		return parent;
	}

	@Override
	public Object getBindingAt(final int index, final int nestingLevel) {
		if (nestingLevel == 0) {
			return boundValues[index];
		} else {
			return parent.getBindingAt(index, nestingLevel - 1);
		}
	}

	@Override
	public void setBindingAt(final int index, final int nestingLevel, final Object value) {
		if (nestingLevel == 0) {
			boundValues[index] = value;
		} else {
			parent.setBindingAt(index, nestingLevel - 1, value);
		}
	}
}
