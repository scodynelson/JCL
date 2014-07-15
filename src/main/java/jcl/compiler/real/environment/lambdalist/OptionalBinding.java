package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class OptionalBinding extends FunctionBinding {

	private final SuppliedPBinding suppliedPBinding;

	public OptionalBinding(final SymbolStruct symbolStruct, final int allocationPosition, final LispStruct initForm,
						   final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
