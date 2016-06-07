/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import jcl.types.ComplexType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;

/**
 * The {@link ComplexStruct2} is the object representation of a Lisp 'complex' type.
 */
public final class ComplexStruct2 extends NumberStruct2Impl<Apcomplex> {

	/**
	 * {@link ComplexStruct2} constant representing I.
	 */
	public static final ComplexStruct2 I = valueOf(Apcomplex.I);

	/**
	 * {@link ComplexStruct2} constant representing -I.
	 */
	public static final ComplexStruct2 NEGATE_I = valueOf(Apcomplex.ZERO, new Apint(-1L));

	/**
	 * {@link ComplexStruct2} constant representing 0.
	 */
	public static final ComplexStruct2 ZERO = valueOf(Apcomplex.ZERO);

	/**
	 * {@link ComplexStruct2} constant representing 0.0.
	 */
	public static final ComplexStruct2 ZERO_FLOAT = valueOf(Apcomplex.ZERO);

	/**
	 * {@link ComplexStruct2} constant representing 1.
	 */
	public static final ComplexStruct2 ONE = valueOf(Apcomplex.ONE);

	/**
	 * {@link ComplexStruct2} constant representing 1.0.
	 */
	public static final ComplexStruct2 ONE_FLOAT = valueOf(Apcomplex.ONE);

	/**
	 * Private constructor.
	 *
	 * @param apcomplex
	 * 		the value of the ComplexStruct2
	 */
	private ComplexStruct2(final Apcomplex apcomplex) {
		super(ComplexType.INSTANCE, apcomplex);
	}

	/**
	 * Returns a new ComplexStruct2 representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new ComplexStruct2
	 *
	 * @return a new ComplexStruct2 representing the provided {@link String}
	 */
	public static ComplexStruct2 valueOf(final String s) {
		final Apcomplex apcomplex = new Apcomplex(s);
		return valueOf(apcomplex);
	}

	/**
	 * Returns a ComplexStruct2 object with the provided {@link Apcomplex} value.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} value of the resulting ComplexStruct2
	 *
	 * @return a ComplexStruct2 object with the provided {@link Apcomplex} value
	 */
	public static ComplexStruct2 valueOf(final Apcomplex apcomplex) {
		return new ComplexStruct2(apcomplex);
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
	public static ComplexStruct2 valueOf(final Apfloat real, final Apfloat imaginary) {
		final Apcomplex apcomplex = new Apcomplex(real, imaginary);
		return valueOf(apcomplex);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return ap.toString();
	}
}
