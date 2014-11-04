package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.Scope;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.T;

public class SuppliedPBinding extends EnvironmentBinding {

	public SuppliedPBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, null);
	}
}
