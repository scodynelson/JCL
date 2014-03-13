package jcl.typespecifiers.compound;

import jcl.types.LispType;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.TypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class EQLTypeSpecifier implements CompoundTypeSpecifier {

	private final TypeSpecifier type;

	public EQLTypeSpecifier(final TypeSpecifier type) {
		this.type = type;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispType)) {
			return false;
		}

		final LispType lispType = (LispType) obj;

		return type.equals(lispType);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().toHashCode();
	}
}
