package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Boolean;

public abstract class BooleanStruct extends SymbolStruct<Boolean> {

	private static final long serialVersionUID = 2558133019376289518L;

	private final boolean booleanValue;

	protected BooleanStruct(final String name, final boolean booleanValue) {
		super(name, GlobalPackageStruct.COMMON_LISP, Boolean.INSTANCE);
		this.booleanValue = booleanValue;
	}

	public boolean booleanValue() {
		return booleanValue;
	}
}
