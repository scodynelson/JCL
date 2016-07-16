/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

public class DecodeFloatResult {

	private final RealStruct significand;

	private final RealStruct exponent;

	private final RealStruct sign;

	public DecodeFloatResult(final RealStruct significand, final RealStruct exponent, final RealStruct sign) {
		this.significand = significand;
		this.exponent = exponent;
		this.sign = sign;
	}

	public RealStruct getSignificand() {
		return significand;
	}

	public RealStruct getExponent() {
		return exponent;
	}

	public RealStruct getSign() {
		return sign;
	}
}
