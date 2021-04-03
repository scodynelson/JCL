package jcl.lang.internal;

import jcl.lang.BooleanStruct;
import jcl.lang.statics.GlobalPackageStruct;

public abstract class BooleanStructImpl extends ConstantStructImpl<BooleanStructImpl> implements BooleanStruct {

	private final boolean booleanValue;

	protected BooleanStructImpl(final String name, final boolean booleanValue) {
		super(name, GlobalPackageStruct.COMMON_LISP, null, null);
		this.booleanValue = booleanValue;

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		value = this;
	}

	@Override
	public boolean toJavaPBoolean() {
		return booleanValue;
	}
}
