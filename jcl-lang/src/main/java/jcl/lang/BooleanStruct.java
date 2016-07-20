package jcl.lang;

import jcl.lang.list.NILStruct;
import jcl.type.LispType;

public abstract class BooleanStruct extends ConstantStruct<BooleanStruct> {

	private final boolean booleanValue;

	protected BooleanStruct(final LispType lispType, final String name, final boolean booleanValue) {
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

	public static BooleanStruct toLispBoolean(final Boolean aBoolean) {
		if (aBoolean == null) {
			return NILStruct.INSTANCE;
		}
		return aBoolean ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
