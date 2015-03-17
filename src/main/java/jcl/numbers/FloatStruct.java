/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.types.Float;
import jcl.types.SingleFloat;

import java.math.BigDecimal;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

	/**
	 * The internal {@link BigDecimal} containing the float contents.
	 */
	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloat.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatFormat
	 * 		a {@link Float} that represents the type of {@link Float}
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final Float floatFormat, final BigDecimal bigDecimal) {
		super(floatFormat, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}
}
