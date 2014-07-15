package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class LetBinding extends Binding {

	private LispStruct initForm;

	public LetBinding(final SymbolStruct symbolStruct, final int allocationPosition, final Scope scope, final LispType type,
					  final LispStruct initForm) {
		super(symbolStruct, new LocalAllocation(allocationPosition), scope, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	public void setInitForm(final LispStruct initForm) {
		this.initForm = initForm;
	}
}
