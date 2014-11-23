/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.numbers;

import jcl.types.Ratio;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigInteger;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	private final BigFraction bigFraction;

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
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the numerator value of the RatioStruct
	 * @param denominator
	 * 		the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		super(Ratio.INSTANCE, null, null);
		bigFraction = new BigFraction(numerator, denominator);
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
	public String printStruct() {
		final IntegerStruct numerator = new IntegerStruct(bigFraction.getNumerator());
		final IntegerStruct denominator = new IntegerStruct(bigFraction.getNumerator());

		return numerator.printStruct() + '/' + denominator.printStruct();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
