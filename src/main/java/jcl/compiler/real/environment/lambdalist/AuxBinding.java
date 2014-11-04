package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.Scope;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.T;

public class AuxBinding extends EnvironmentBinding {

	public AuxBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final LispStruct initForm) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
	}
}
