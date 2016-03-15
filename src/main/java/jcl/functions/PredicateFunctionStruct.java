package jcl.functions;

import org.apache.commons.collections4.Predicate;

public abstract class PredicateFunctionStruct<T> extends FunctionStruct implements Predicate<T> {

	protected PredicateFunctionStruct(final String documentation) {
		super(documentation);
	}

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
