/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

public class QuotientRemainder2 {

	private final RealStruct2 quotient;

	private final RealStruct2 remainder;

	public QuotientRemainder2(final RealStruct2 quotient, final RealStruct2 remainder) {
		this.quotient = quotient;
		this.remainder = remainder;
	}

	public RealStruct2 getQuotient() {
		return quotient;
	}

	public RealStruct2 getRemainder() {
		return remainder;
	}
}
