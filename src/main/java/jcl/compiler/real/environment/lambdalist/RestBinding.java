package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.List;

public class RestBinding extends EnvironmentBinding {

	public RestBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, List.INSTANCE, null);
	}
}
