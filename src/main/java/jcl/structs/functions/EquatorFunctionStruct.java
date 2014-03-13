package jcl.structs.functions;

import jcl.structs.FunctionStruct;
import org.apache.commons.collections4.Equator;

public class EquatorFunctionStruct<T> extends FunctionStruct implements Equator<T> {

	@Override
	public boolean equate(final T o1, final T o2) {
		return (o1 == o2) || ((o1 != null) && o1.equals(o2));
	}

	@Override
	public int hash(final T o) {
		return (o == null) ? -1 : o.hashCode();
	}
}
