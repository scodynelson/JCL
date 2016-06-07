/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import jcl.types.ComplexType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link ComplexStruct2} is the object representation of a Lisp 'complex' type.
 */
public final class ComplexStruct2 extends NumberStruct2Impl<Apcomplex> {

	/**
	 * {@link ComplexStruct2} constant representing I.
	 */
	public static final ComplexStruct2 I = valueOf(Apcomplex.I, ComplexValueType.INTEGER);

	/**
	 * {@link ComplexStruct2} constant representing -I.
	 */
	public static final ComplexStruct2 NEGATE_I = valueOf(Apcomplex.ZERO, new Apint(-1L), ComplexValueType.INTEGER);

	/**
	 * {@link ComplexStruct2} constant representing 0.
	 */
	public static final ComplexStruct2 ZERO = valueOf(Apcomplex.ZERO, ComplexValueType.INTEGER);

	/**
	 * {@link ComplexStruct2} constant representing 0.0.
	 */
	public static final ComplexStruct2 ZERO_FLOAT = valueOf(Apcomplex.ZERO, ComplexValueType.FLOAT);

	/**
	 * {@link ComplexStruct2} constant representing 1.
	 */
	public static final ComplexStruct2 ONE = valueOf(Apcomplex.ONE, ComplexValueType.INTEGER);

	/**
	 * {@link ComplexStruct2} constant representing 1.0.
	 */
	public static final ComplexStruct2 ONE_FLOAT = valueOf(Apcomplex.ONE, ComplexValueType.FLOAT);

	public enum ComplexValueType {
		INTEGER,
		FLOAT
	}

	private final ComplexValueType valueType;

	/**
	 * Private constructor.
	 *
	 * @param apcomplex
	 * 		the value of the ComplexStruct2
	 */
	private ComplexStruct2(final Apcomplex apcomplex, final ComplexValueType valueType) {
		super(ComplexType.INSTANCE, apcomplex);
		this.valueType = valueType;
	}

	/**
	 * Returns a ComplexStruct2 object with the provided {@link Apcomplex} value.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} value of the resulting ComplexStruct2
	 *
	 * @return a ComplexStruct2 object with the provided {@link Apcomplex} value
	 */
	public static ComplexStruct2 valueOf(final Apcomplex apcomplex, final ComplexValueType valueType) {
		return new ComplexStruct2(apcomplex, valueType);
	}

	/**
	 * Returns a ComplexStruct2 object with the provided real and imaginary {@link Apfloat} values.
	 *
	 * @param real
	 * 		the {@link Apfloat} value of the real of the resulting ComplexStruct2
	 * @param imaginary
	 * 		the {@link Apfloat} value of the imaginary of the resulting ComplexStruct2
	 *
	 * @return a ComplexStruct2 object with the provided real and imaginary {@link Apfloat} values
	 */
	public static ComplexStruct2 valueOf(final Apfloat real, final Apfloat imaginary, final ComplexValueType valueType) {
		final Apcomplex apcomplex = new Apcomplex(real, imaginary);
		return valueOf(apcomplex, valueType);
	}

	public static ComplexValueType determineComplexValueType(final RealStruct2... reals) {
		return ComplexValueType.INTEGER;
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return "#C(" + formatApfloat(ap.real()) + ' ' + formatApfloat(ap.imag()) + ')';
	}

	private String formatApfloat(final Apfloat apfloat) {
		switch (valueType) {
			case FLOAT:
				if (apfloat instanceof Aprational) {
					return String.valueOf(apfloat.doubleValue());
				}
		}
		return apfloat.toString(true);
	}
}
