package jcl.symbols;

import jcl.LispStruct;

public abstract class BooleanStruct<T extends LispStruct> extends SymbolStruct<T> {

	private final boolean booleanValue;

	protected BooleanStruct(final String name, final boolean booleanValue) {
		super(name);
		this.booleanValue = booleanValue;
	}

	public boolean booleanValue() {
		return booleanValue;
	}
}
