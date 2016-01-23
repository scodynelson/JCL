package jcl.functions;

import java.util.Objects;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import org.apache.commons.collections4.Equator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public abstract class EquatorFunctionStruct extends FunctionStruct implements Equator<LispStruct> {

	protected EquatorFunctionStruct(final String documentation) {
		super(documentation);
	}

	protected EquatorFunctionStruct(final String documentation, final OrdinaryLambdaList lambdaListBindings) {
		super(documentation, lambdaListBindings);
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
