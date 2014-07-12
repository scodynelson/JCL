package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class LambdaBinding extends Binding {

	public LambdaBinding(final SymbolStruct symbolStruct, final int allocationPosition, final Scope scope,
						 final LispType type, final boolean isRequired) {
		super(symbolStruct, new ParameterAllocation(allocationPosition), scope, type, isRequired);
	}
}
