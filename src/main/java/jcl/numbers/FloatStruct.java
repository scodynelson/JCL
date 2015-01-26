/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.reader.struct.ReaderVariables;
import jcl.types.DoubleFloat;
import jcl.types.Float;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigDecimal;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

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

	@Override
	public String printStruct() {
		final Float floatFormat = (Float) getType();
		final Float defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getValue();

		String bigDecimalString = bigDecimal.toString();
		if (!floatFormat.equals(defaultFloatFormat)) {
			if (floatFormat.equals(ShortFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'S');
			} else if (floatFormat.equals(SingleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'F');
			} else if (floatFormat.equals(DoubleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'D');
			} else if (floatFormat.equals(LongFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'L');
			}
		}

		return bigDecimalString;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
