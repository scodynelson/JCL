/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import java.util.Arrays;

import jcl.lang.RealStruct;
import jcl.type.ComplexType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public final class ComplexStruct extends NumberStructImpl<Apcomplex> {

	/**
	 * {@link ComplexStruct} constant representing I.
	 */
	public static final ComplexStruct I = valueOf(Apcomplex.I, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing -I.
	 */
	public static final ComplexStruct NEGATE_I = valueOf(Apcomplex.ZERO, new Apint(-1L), ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 0.
	 */
	public static final ComplexStruct ZERO = valueOf(Apcomplex.ZERO, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 0.0.
	 */
	public static final ComplexStruct ZERO_FLOAT = valueOf(Apcomplex.ZERO, ValueType.FLOAT);

	/**
	 * {@link ComplexStruct} constant representing 1.
	 */
	public static final ComplexStruct ONE = valueOf(Apcomplex.ONE, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 1.0.
	 */
	public static final ComplexStruct ONE_FLOAT = valueOf(Apcomplex.ONE, ValueType.FLOAT);

	public enum ValueType {
		RATIONAL,
		FLOAT
	}

	private final ValueType valueType;

	/**
	 * Private constructor.
	 *
	 * @param apcomplex
	 * 		the value of the ComplexStruct
	 */
	private ComplexStruct(final Apcomplex apcomplex, final ValueType valueType) {
		super(ComplexType.INSTANCE, apcomplex);
		this.valueType = valueType;
	}

	public ValueType getValueType() {
		return valueType;
	}

	/**
	 * Returns a ComplexStruct object with the provided {@link Apcomplex} value.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} value of the resulting ComplexStruct
	 *
	 * @return a ComplexStruct object with the provided {@link Apcomplex} value
	 */
	public static ComplexStruct valueOf(final Apcomplex apcomplex, final ValueType valueType) {
		return new ComplexStruct(apcomplex, valueType);
	}

	/**
	 * Returns a ComplexStruct object with the provided real and imaginary {@link Apfloat} values.
	 *
	 * @param real
	 * 		the {@link Apfloat} value of the real of the resulting ComplexStruct
	 * @param imaginary
	 * 		the {@link Apfloat} value of the imaginary of the resulting ComplexStruct
	 *
	 * @return a ComplexStruct object with the provided real and imaginary {@link Apfloat} values
	 */
	public static ComplexStruct valueOf(final Apfloat real, final Apfloat imaginary, final ValueType valueType) {
		final Apcomplex apcomplex = new Apcomplex(real, imaginary);
		return valueOf(apcomplex, valueType);
	}

	public static ComplexStruct valueOf(final RealStruct real, final RealStruct imaginary) {
		final ValueType valueType = determineComplexValueType(real, imaginary);
		return valueOf(real.ap(), imaginary.ap(), valueType);
	}

	public static ValueType determineComplexValueType(final RealStruct... reals) {
		final boolean anyFloats
				= Arrays.stream(reals)
				        .map(Object::getClass)
				        .anyMatch(FloatStruct.class::isAssignableFrom);

		return anyFloats ? ValueType.FLOAT : ValueType.RATIONAL;
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
