/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types.typespecifiers;

import jcl.LispType;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * An {@link EQLTypeSpecifier} represents the type of all x for which (eql object x) is true. The argument object is
 * required. The object can be *, but if so it denotes itself (the symbol *) and does not represent an unspecified
 * value. The symbol eql is not valid as an atomic type specifier.
 */
public class EQLTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The {@link TypeSpecifier} to check against the 'EQL' type specifier.
	 */
	private final TypeSpecifier typeSpecifier;

	/**
	 * Public constructor.
	 *
	 * @param typeSpecifier
	 * 		the type specifier to test equality
	 */
	public EQLTypeSpecifier(final TypeSpecifier typeSpecifier) {
		this("T", typeSpecifier); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param typeSpecifier
	 * 		the type specifier to test equality
	 */
	protected EQLTypeSpecifier(final String name, final TypeSpecifier typeSpecifier) {
		super(name);
		this.typeSpecifier = typeSpecifier;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(typeSpecifier)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispType)) {
			return false;
		}

		// TODO: does this account for x.equals(y) and y.equals(x)???
		final LispType lispType = (LispType) obj;
		return typeSpecifier.equals(lispType);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(typeSpecifier)
		                                                                .toString();
	}
}
