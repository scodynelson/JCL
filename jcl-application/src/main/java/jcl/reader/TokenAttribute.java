/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Used to store a character token and its {@link AttributeType} value as a {@link Reader} process executes.
 */
public class TokenAttribute {

	/**
	 * The character token code point.
	 */
	private final int codePoint;

	/**
	 * The {@link AttributeType} of the {@link #codePoint}.
	 */
	private final AttributeType attributeType;

	/**
	 * Package private constructor.
	 *
	 * @param codePoint
	 * 		the character token code point
	 * @param attributeType
	 * 		the {@link AttributeType} of the character token
	 */
	TokenAttribute(final int codePoint, final AttributeType attributeType) {
		this.codePoint = codePoint;
		this.attributeType = attributeType;
	}

	/**
	 * Getter for {@link #codePoint} property.
	 *
	 * @return {@link #codePoint} property
	 */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Getter for {@link #attributeType} property.
	 *
	 * @return {@link #attributeType} property
	 */
	public AttributeType getAttributeType() {
		return attributeType;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(codePoint)
		                            .append(attributeType)
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
		final TokenAttribute rhs = (TokenAttribute) obj;
		return new EqualsBuilder().append(codePoint, rhs.codePoint)
		                          .append(attributeType, rhs.attributeType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(codePoint)
		                                                                .append(attributeType)
		                                                                .toString();
	}
}
