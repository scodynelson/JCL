package jcl.structs.functions;

import jcl.structs.FunctionStruct;
import org.apache.commons.collections4.Predicate;

public class PredicateFunctionStruct<T> extends FunctionStruct implements Predicate<T> {

	@Override
	public boolean evaluate(final T object) {
		return false;
	}
}
