/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

public class QuotientRemainderResult {

	private final RealStruct quotient;

	private final RealStruct remainder;

	public QuotientRemainderResult(final RealStruct quotient, final RealStruct remainder) {
		this.quotient = quotient;
		this.remainder = remainder;
	}

	public RealStruct getQuotient() {
		return quotient;
	}

	public RealStruct getRemainder() {
		return remainder;
	}
}
