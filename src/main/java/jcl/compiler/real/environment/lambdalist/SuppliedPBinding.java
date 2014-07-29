package jcl.compiler.real.environment.lambdalist;

import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class SuppliedPBinding extends FunctionBinding {

	public SuppliedPBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, null);
	}
}
