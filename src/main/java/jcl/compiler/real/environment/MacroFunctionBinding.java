package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.structs.symbols.SymbolStruct;

public class MacroFunctionBinding extends LambdaBinding {

	private SymbolStruct<?> name;

	public MacroFunctionBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final Scope scope, final LispType type,
								final SymbolStruct<?> name) {
		super(symbolStruct, allocationPosition, scope, type);
		this.name = name;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public void setName(final SymbolStruct<?> name) {
		this.name = name;
	}
}
