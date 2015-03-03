/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigInteger;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4665072618932472349L;

	/**
	 * The internal {@link BigInteger} containing the float contents.
	 */
	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(jcl.types.Integer.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerFormat
	 * 		a {@link jcl.types.Integer} that represents the type of {@link jcl.types.Integer}
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final jcl.types.Integer integerFormat, final BigInteger bigInteger) {
		super(integerFormat, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
