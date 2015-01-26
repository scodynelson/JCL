package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Boolean;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
