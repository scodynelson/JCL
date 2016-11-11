package jcl.lang.function;

import org.apache.commons.collections4.Predicate;

public abstract class PredicateFunctionStructBase<T> extends FunctionStructImpl implements Predicate<T> {

	protected PredicateFunctionStructBase(final String documentation) {
		super(documentation);
	}

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
