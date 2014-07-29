package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.List;

public class BodyBinding extends FunctionBinding {

	public BodyBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final LispStruct initForm) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, List.INSTANCE, initForm);
	}
}
