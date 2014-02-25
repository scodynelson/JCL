package jcl.structs.functions;

import org.apache.commons.collections4.Predicate;

public class PredicateFunctionStruct<T> extends FunctionStruct implements Predicate<T> {

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
