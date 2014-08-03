package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.LispType;
import jcl.structs.symbols.SymbolStruct;

public class FunctionBinding extends LambdaBinding {

	private LispStruct initForm;

	public FunctionBinding(final SymbolStruct<?> symbolStruct, final int allocationPosition, final Scope scope, final LispType type,
						   final LispStruct initForm) {
		super(symbolStruct, allocationPosition, scope, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	public void setInitForm(final LispStruct initForm) {
		this.initForm = initForm;
	}
}
