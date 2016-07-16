package jcl.lang.function;

import java.util.Objects;

import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Parameters;
import org.apache.commons.collections4.Equator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public abstract class EquatorFunctionStruct extends CommonLispBuiltInFunctionStruct implements Equator<LispStruct> {

	protected EquatorFunctionStruct(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	@Override
	public boolean equate(final LispStruct o1, final LispStruct o2) {
		return Objects.equals(o1, o2);
	}

	@Override
	public int hash(final LispStruct o) {
		return new HashCodeBuilder().append(o)
		                            .toHashCode();
	}
}
