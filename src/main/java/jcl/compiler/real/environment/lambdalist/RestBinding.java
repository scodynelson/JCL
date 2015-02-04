package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.List;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class RestBinding extends ParameterBinding {

	private static final long serialVersionUID = 5070599837585531277L;

	public RestBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation) {
		super(symbolStruct, allocation, Scope.LEXICAL, List.INSTANCE, null);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
