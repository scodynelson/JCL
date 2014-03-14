package jcl.functions;

import jcl.structs.FunctionStruct;
import org.apache.commons.collections4.Equator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.Objects;

public abstract class EquatorFunctionStruct<T> extends FunctionStruct implements Equator<T> {

	@Override
	public boolean equate(final T o1, final T o2) {
		return Objects.equals(o1, o2);
	}

	@Override
	public int hash(final T o) {
		return new HashCodeBuilder()
				.append(o)
				.toHashCode();
	}
}
