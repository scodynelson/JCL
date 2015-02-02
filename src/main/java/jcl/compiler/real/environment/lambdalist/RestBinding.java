package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.List;

public class RestBinding extends EnvironmentBinding {

	public RestBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation) {
		super(symbolStruct, allocation, Scope.LEXICAL, List.INSTANCE, null);
	}
}
