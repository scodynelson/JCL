package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.Scope;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.List;

public class WholeBinding extends EnvironmentBinding {

	public WholeBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final LispStruct initForm) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, List.INSTANCE, initForm);
	}
}
