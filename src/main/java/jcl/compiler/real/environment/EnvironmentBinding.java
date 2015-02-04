package jcl.compiler.real.environment;

import jcl.LispType;
import jcl.compiler.real.element.Element;
import jcl.symbols.SymbolStruct;

public class EnvironmentBinding extends Binding {

	private static final long serialVersionUID = 2910922877559341453L;

	private final Element initForm;

	public EnvironmentBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final Scope scope,
	                          final LispType type, final Element initForm) {
		super(symbolStruct, allocation, scope, type);
		this.initForm = initForm;
	}

	public Element getInitForm() {
		return initForm;
	}
}
