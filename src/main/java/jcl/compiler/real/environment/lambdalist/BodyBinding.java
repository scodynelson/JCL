package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.List;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class BodyBinding extends ParameterBinding {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm) {
		super(symbolStruct, allocation, Scope.LEXICAL, List.INSTANCE, initForm);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
