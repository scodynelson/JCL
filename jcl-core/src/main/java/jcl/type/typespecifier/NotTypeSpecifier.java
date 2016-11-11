/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import jcl.type.LispType;
import jcl.type.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * A {@link NotTypeSpecifier} denotes the set of all objects that are not of the type typespec.
 * The symbol not is not valid as a type specifier.
 */
public class NotTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The {@link LispType} to 'NOT'.
	 */
	private final LispType type;

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param type
	 * 		a {@link LispType}
	 */
	public NotTypeSpecifier(final LispType type) {
		this("T", type); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param type
	 * 		a {@link LispType}
	 */
	protected NotTypeSpecifier(final String name, final LispType type) {
		super(name);
		this.type = type;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(type)
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

		final LispType lispType = (LispType) obj;

		return !lispType.equals(type) && !type.equals(lispType);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(type)
		                                                                .toString();
	}
}