package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class RequiredBinding extends EnvironmentBinding {

	public RequiredBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, null);
	}
}
