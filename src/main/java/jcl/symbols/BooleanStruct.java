package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.BooleanType;

public abstract class BooleanStruct extends ConstantStruct<BooleanStruct> {

	private final boolean booleanValue;

	protected BooleanStruct(final String name, final boolean booleanValue) {
		super(BooleanType.INSTANCE, name, GlobalPackageStruct.COMMON_LISP, null, null);
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
