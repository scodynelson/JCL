package jcl.symbols;

import jcl.LispStruct;
import jcl.packages.GlobalPackageStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class BooleanStruct<T extends LispStruct> extends SymbolStruct<T> {

	private final boolean booleanValue;

	protected BooleanStruct(final String name, final T value, final boolean booleanValue) {
		super(name, GlobalPackageStruct.COMMON_LISP, value);
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
