/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import jcl.lang.ComplexStruct;
import jcl.lang.RealStruct;
import jcl.type.ComplexType;
import lombok.EqualsAndHashCode;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;

/**
 * The {@link ComplexStructImpl} is the object representation of a Lisp 'complex' type.
 */
@Deprecated
@EqualsAndHashCode(callSuper = true)
public final class ComplexStructImpl extends NumberStructImpl<Apcomplex> implements ComplexStruct {

	/**
	 * Private constructor.
	 *
	 * @param apcomplex
	 * 		the value of the ComplexStruct
	 */
	private ComplexStructImpl(final Apcomplex apcomplex) {
		super(ComplexType.INSTANCE, apcomplex);
	}

	/**
	 * Returns a ComplexStruct object with the provided {@link Apcomplex} value.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} value of the resulting ComplexStruct
	 *
	 * @return a ComplexStruct object with the provided {@link Apcomplex} value
	 */
	@Deprecated
	public static ComplexStruct valueOf(final Apcomplex apcomplex) {
		return new ComplexStructImpl(apcomplex);
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
	@Deprecated
	public static ComplexStruct valueOf(final Apfloat real, final Apfloat imaginary) {
		final Apcomplex apcomplex = new Apcomplex(real, imaginary);
		return valueOf(apcomplex);
	}

	@Deprecated
	public static ComplexStruct valueOf(final RealStruct real, final RealStruct imaginary) {
		return valueOf(real.ap(), imaginary.ap());
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return "#C(" + formatApfloat(ap.real()) + ' ' + formatApfloat(ap.imag()) + ')';
	}

	private String formatApfloat(final Apfloat apfloat) {
		return apfloat.toString(true);
	}
}
