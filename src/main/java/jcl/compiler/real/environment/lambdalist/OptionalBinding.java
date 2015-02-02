package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.EnvironmentBinding;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class OptionalBinding extends EnvironmentBinding {

	private final SuppliedPBinding suppliedPBinding;

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm,
						   final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocation, Scope.LEXICAL, T.INSTANCE, initForm);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
