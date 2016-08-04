package jcl.lang.internal;

import jcl.lang.BooleanStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.LispType;

public abstract class BooleanStructImpl extends ConstantStructImpl<BooleanStructImpl> implements BooleanStruct {

	private final boolean booleanValue;

	protected BooleanStructImpl(final LispType lispType, final String name, final boolean booleanValue) {
		super(lispType, name, GlobalPackageStruct.COMMON_LISP, null, null);
		this.booleanValue = booleanValue;

		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		dynamicValueStack.push(this);
	}

	@Override
	public boolean booleanValue() {
		return booleanValue;
	}
}
