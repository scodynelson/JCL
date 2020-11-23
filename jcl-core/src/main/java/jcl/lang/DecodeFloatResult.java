/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Result wrapper object for {@link FloatStruct} float decoding operations.
 */
@Getter
@AllArgsConstructor
public final class DecodeFloatResult {

	/**
	 * Significand value result for {@link FloatStruct} float decoding operation.
	 */
	private final RealStruct significand;

	/**
	 * Exponent value result for {@link FloatStruct} float decoding operation.
	 */
	private final RealStruct exponent;

	/**
	 * Sign value result for {@link FloatStruct} float decoding operation.
	 */
	private final RealStruct sign;

	/**
	 * Returns a {@link ValuesStruct} containing the {@link #significand}, {@link #exponent}, and {@link #sign} values.
	 *
	 * @return a {@link ValuesStruct} containing the {@link #significand}, {@link #exponent}, and {@link #sign} values
	 */
	public ValuesStruct toValues() {
		return ValuesStruct.valueOf(significand, exponent, sign);
	}
}
