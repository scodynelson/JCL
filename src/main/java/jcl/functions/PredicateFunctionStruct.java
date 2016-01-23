package jcl.functions;

import org.apache.commons.collections4.Predicate;

public abstract class PredicateFunctionStruct<T> extends FunctionStruct implements Predicate<T> {

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
