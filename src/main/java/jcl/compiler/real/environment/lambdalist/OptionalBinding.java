package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.FunctionBinding;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.sa.LambdaListParser;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class OptionalBinding extends FunctionBinding {

	private final SymbolStruct<?> varName;

	public OptionalBinding(final int allocationPosition, final LispStruct initForm, SymbolStruct<?> varName) {
		super(LambdaListParser.OPTIONAL, allocationPosition, Scope.LEXICAL, T.INSTANCE, initForm);
		this.varName = varName;
	}

	public SymbolStruct<?> getVarName() {
		return varName;
	}
}
