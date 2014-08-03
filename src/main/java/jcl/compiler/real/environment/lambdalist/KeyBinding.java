package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.structs.symbols.KeywordSymbolStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.T;

public class KeyBinding extends FunctionBinding {

	private final KeywordSymbolStruct keyName;
	private final SuppliedPBinding suppliedPBinding;

	public KeyBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final LispStruct initForm,
					  final KeywordSymbolStruct keyName, final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public KeywordSymbolStruct getKeyName() {
		return keyName;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
