package jcl.functions;

import org.apache.commons.collections4.Predicate;

public abstract class PredicateFunctionStruct<T> extends FunctionStruct implements Predicate<T> {

	private static final long serialVersionUID = -8127036645516138198L;

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
