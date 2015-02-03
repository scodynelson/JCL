package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class EnvironmentBinding extends Binding {

	private static final long serialVersionUID = 2910922877559341453L;

	private final LispStruct initForm;

	public EnvironmentBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final Scope scope,
	                          final LispType type, final LispStruct initForm) {
		super(symbolStruct, allocation, scope, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}
}
