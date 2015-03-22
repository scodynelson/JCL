/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;

import jcl.types.Ratio;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	private final BigFraction bigFraction;

	/**
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the numerator value of the RatioStruct
	 * @param denominator
	 * 		the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		this(new BigFraction(numerator, denominator));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(Ratio.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigFraction)
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
		final RatioStruct rhs = (RatioStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigFraction)
		                                                                .toString();
	}
}
