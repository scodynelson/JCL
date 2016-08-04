package jcl.lang;

import jcl.lang.internal.ConstantStructImpl;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.type.LispType;

public abstract class BooleanStructImpl extends ConstantStructImpl<BooleanStructImpl> {

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

	public boolean booleanValue() {
		return booleanValue;
	}

	public static BooleanStructImpl toLispBoolean(final Boolean aBoolean) {
		if (aBoolean == null) {
			return NILStruct.INSTANCE;
		}
		return aBoolean ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
