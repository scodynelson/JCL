package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class FunctionBinding extends LambdaBinding {

	private SymbolStruct name;

	public FunctionBinding(final SymbolStruct symbolStruct, final int allocationPosition, final Scope scope,
						   final LispType type, final SymbolStruct name, final boolean isRequired) {
		super(symbolStruct, allocationPosition, scope, type, isRequired);
		this.name = name;
	}

	public SymbolStruct getName() {
		return name;
	}

	public void setName(final SymbolStruct name) {
		this.name = name;
	}
}
