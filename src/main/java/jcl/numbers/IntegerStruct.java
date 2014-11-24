/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.symbols.variables.Variable;
import jcl.types.Integer;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigInteger;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * Int constant for the value '2'.
	 */
	private static final int TWO = 2;
	/**
	 * Int constant for the value '8'.
	 */
	private static final int EIGHT = 8;
	/**
	 * Int constant for the value '10'.
	 */
	private static final int TEN = 10;
	/**
	 * Int constant for the value '16'.
	 */
	private static final int SIXTEEN = 16;
	/**
	 * The internal {@link BigInteger} containing the float contents.
	 */
	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(Integer.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerFormat
	 * 		a {@link Integer} that represents the type of {@link Integer}
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final Integer integerFormat, final BigInteger bigInteger) {
		super(integerFormat, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public String printStruct() {
		final boolean printRadix = Variable.PRINT_RADIX.getValue().booleanValue();
		final int printBase = Variable.PRINT_BASE.getValue().bigInteger.intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == TWO) {
				stringBuilder.append("#b");
			} else if (printBase == EIGHT) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == TEN)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
