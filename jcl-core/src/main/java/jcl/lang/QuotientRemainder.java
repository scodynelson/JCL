/*
 * Copyright (c) 2011-2020 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Result wrapper object for {@link RealStruct} numeric rounding operations.
 */
@Getter
@AllArgsConstructor
public final class QuotientRemainder {

	/**
	 * Quotient value result for {@link RealStruct} numeric rounding operation.
	 */
	private final RealStruct quotient;

	/**
	 * Remainder value result for {@link RealStruct} numeric rounding operation.
	 */
	private final RealStruct remainder;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #quotient} and {@link #remainder} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #quotient} and {@link #remainder} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(quotient, remainder);
	}
}
