/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

public class DecodeFloatResult2 {

	private final RealStruct2 significand;

	private final RealStruct2 exponent;

	private final RealStruct2 sign;

	public DecodeFloatResult2(final RealStruct2 significand, final RealStruct2 exponent, final RealStruct2 sign) {
		this.significand = significand;
		this.exponent = exponent;
		this.sign = sign;
	}

	public RealStruct2 getSignificand() {
		return significand;
	}

	public RealStruct2 getExponent() {
		return exponent;
	}

	public RealStruct2 getSign() {
		return sign;
	}
}
