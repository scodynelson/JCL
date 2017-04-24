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
import org.apfloat.Aprational;

/**
 * The {@link ComplexStructImpl} is the object representation of a Lisp 'complex' type.
 */
@EqualsAndHashCode(callSuper = true)
public final class ComplexStructImpl extends NumberStructImpl<Apcomplex> implements ComplexStruct {

	private final ValueType valueType;

	/**
	 * Private constructor.
	 *
	 * @param apcomplex
	 * 		the value of the ComplexStruct
	 */
	private ComplexStructImpl(final Apcomplex apcomplex, final ValueType valueType) {
		super(ComplexType.INSTANCE, apcomplex);
		this.valueType = valueType;
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
		return new ComplexStructImpl(apcomplex, valueType);
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
		final ValueType valueType = ComplexStruct.determineComplexValueType(real, imaginary);
		return valueOf(real.ap(), imaginary.ap(), valueType);
	}

	@Override
	public ValueType getValueType() {
		return valueType;
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
