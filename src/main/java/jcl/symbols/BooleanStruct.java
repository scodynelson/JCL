package jcl.symbols;

import jcl.LispType;
import jcl.packages.GlobalPackageStruct;

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
}
