package jcl.structs.symbols;

import jcl.LispStruct;
import jcl.structs.packages.GlobalPackageStruct;

public abstract class BooleanStruct<T extends LispStruct> extends SymbolStruct<T> {

	private final boolean booleanValue;

	protected BooleanStruct(final String name, final T value, final boolean booleanValue) {
		super(name, GlobalPackageStruct.COMMON_LISP, value);
		this.booleanValue = booleanValue;
	}

	public boolean booleanValue() {
		return booleanValue;
	}
}
