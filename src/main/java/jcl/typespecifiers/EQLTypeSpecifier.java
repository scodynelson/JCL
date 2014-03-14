package jcl.typespecifiers;

import jcl.LispType;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@code EQLTypeSpecifier} represents the type of all x for which (eql object x) is true. The argument object is required.
 * The object can be *, but if so it denotes itself (the symbol *) and does not represent an unspecified value. The symbol
 * eql is not valid as an atomic type specifier.
 */
public class EQLTypeSpecifier implements CompoundTypeSpecifier {

	private final TypeSpecifier typeSpecifier;

	/**
	 * Public constructor.
	 *
	 * @param typeSpecifier the type specifier to test equality
	 */
	public EQLTypeSpecifier(final TypeSpecifier typeSpecifier) {
		this.typeSpecifier = typeSpecifier;
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
		return typeSpecifier.equals(lispType);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(typeSpecifier)
				.toHashCode();
	}
}
