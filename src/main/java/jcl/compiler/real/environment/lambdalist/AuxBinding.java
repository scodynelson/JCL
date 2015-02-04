package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class AuxBinding extends ParameterBinding {

	private static final long serialVersionUID = 6424631350711831345L;

	public AuxBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm) {
		super(symbolStruct, allocation, Scope.LEXICAL, T.INSTANCE, initForm);
	}
}
