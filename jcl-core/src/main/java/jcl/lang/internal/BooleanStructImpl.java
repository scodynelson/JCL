package jcl.lang.internal;

import jcl.lang.BooleanStruct;

public abstract class BooleanStructImpl extends SymbolStructImpl implements BooleanStruct {

	private final boolean booleanValue;

	protected BooleanStructImpl(final String name, final boolean booleanValue) {
		super(name);
		this.booleanValue = booleanValue;
	}

	@Override
	public boolean toJavaPBoolean() {
		return booleanValue;
	}
}
