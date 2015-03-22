package jcl.symbols;

import jcl.packages.GlobalPackageStruct;
import jcl.types.Boolean;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
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
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(booleanValue)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final BooleanStruct rhs = (BooleanStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(booleanValue, rhs.booleanValue)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(booleanValue)
		                                                                .toString();
	}
}
