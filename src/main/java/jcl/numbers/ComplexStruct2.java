/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.types.ComplexType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;

/**
 * The {@link ComplexStruct2} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct2 extends NumberStruct2Impl<Apcomplex> {

	/**
	 * {@link ComplexStruct2} constant representing I.
	 */
	public static final ComplexStruct2 I = new ComplexStruct2(IntegerStruct2.ZERO, IntegerStruct2.ONE);

	/**
	 * {@link ComplexStruct2} constant representing -I.
	 */
	public static final ComplexStruct2 NEGATE_I = new ComplexStruct2(IntegerStruct2.ZERO, IntegerStruct2.MINUS_ONE);

	/**
	 * {@link ComplexStruct2} constant representing 0.
	 */
	public static final ComplexStruct2 ZERO = new ComplexStruct2(IntegerStruct2.ZERO, IntegerStruct2.ZERO);

	/**
	 * {@link ComplexStruct2} constant representing 0.0.
	 */
	public static final ComplexStruct2 ZERO_FLOAT = new ComplexStruct2(FloatStruct2.ZERO, FloatStruct2.ZERO);

	/**
	 * {@link ComplexStruct2} constant representing 1.
	 */
	public static final ComplexStruct2 ONE = new ComplexStruct2(IntegerStruct2.ONE, IntegerStruct2.ZERO);

	/**
	 * {@link ComplexStruct2} constant representing 1.0.
	 */
	public static final ComplexStruct2 ONE_FLOAT = new ComplexStruct2(FloatStruct2.ONE, FloatStruct2.ZERO);

	/**
	 * The {@link RealStruct} that comprises the real value of the complex.
	 */
	private final RealStruct2 real;

	/**
	 * The {@link RealStruct} that comprises the imaginary value of the complex.
	 */
	private final RealStruct2 imaginary;

	private final Apcomplex apcomplex;

	//	/**
//	 * Public constructor.
//	 *
//	 * @param real
//	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
//	 * @param imaginary
//	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
//	 */
	ComplexStruct2(final Apcomplex apcomplex) {
		this(apcomplex.real(), apcomplex.imag());
	}

	//	/**
//	 * Public constructor.
//	 *
//	 * @param real1
//	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
//	 * @param imaginary2
//	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
//	 */
	ComplexStruct2(final Apfloat real, final Apfloat imaginary) {
		this(toRealStruct(real), toRealStruct(imaginary));
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final RealStruct2 real, final RealStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);

		RealStruct2 coercedReal = real;
		RealStruct2 coercedImaginary = imaginary;
		if ((real instanceof FloatStruct) || (imaginary instanceof FloatStruct)) {
			coercedReal = real.floatingPoint();
			coercedImaginary = imaginary.floatingPoint();
		}
		this.real = coercedReal;
		this.imaginary = coercedImaginary;

		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final IntegerStruct2 real, final IntegerStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final IntegerStruct2 real, final FloatStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real.floatingPoint();
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final IntegerStruct2 real, final RatioStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final FloatStruct2 real, final IntegerStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary.floatingPoint();
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final FloatStruct2 real, final FloatStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final FloatStruct2 real, final RatioStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary.floatingPoint();
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final RatioStruct2 real, final IntegerStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final RatioStruct2 real, final FloatStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real.floatingPoint();
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct2(final RatioStruct2 real, final RatioStruct2 imaginary) {
		super(ComplexType.INSTANCE, null);
		this.real = real;
		this.imaginary = imaginary;
		this.apcomplex = null;
	}

	private static RealStruct2 toRealStruct(final Apfloat apfloat) {
		// TODO: Not quite right here either!!!
//		return (apfloat.doubleValue() == apfloat.intValue()) ? IntegerStruct2.valueOf(apfloat) : FloatStruct2.valueOf(apfloat);
		return null;
	}

	public static ComplexStruct2 valueOf(final Apcomplex apcomplex) {
		return new ComplexStruct2(apcomplex);
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct2 getReal() {
		return real;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct2 getImaginary() {
		return imaginary;
	}

	@Override
	public Apcomplex ap() {
		return ap;
	}

	@Override
	public RealStruct2 abs() {
//		if (real.zerop()) {
//			return imaginary.abs();
//		}
//
//		final Apcomplex apcomplex = apcomplexValue();
//		final Apfloat abs = ApcomplexMath.abs(apcomplex);
//		return toRealStruct(abs);
		return null;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this ComplexStruct is zero using {@link RealStruct#zerop} on {@link #real} and {@link
	 * #imaginary}.
	 */
	@Override
	public boolean zerop() {
//		return real.zerop() && imaginary.zerop();
		return false;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		return false;
	}

	@Override
	public NumberStruct2 signum() {
		if (zerop()) {
			return this;
		}

		final RealStruct2 abs = abs();
		return divide(abs);
	}

	@Override
	public RealStruct2 realPart() {
		return real;
	}

	@Override
	public RealStruct2 imagPart() {
		return imaginary;
	}

	@Override
	public NumberStruct2 conjugate() {
//		final NumberStruct2 imagNegation = imaginary.negation();
//		return makeComplexOrReal(real, (RealStruct) imagNegation);
		return null;
	}

	@Override
	public NumberStruct2 negation() {
//		return makeComplexOrReal((RealStruct) real.negation(), (RealStruct) imaginary.negation());
		return null;
	}

	@Override
	public NumberStruct2 reciprocal() {
		return ONE.divide(this);
	}

	@Override
	public NumberStruct2 exp() {
//		if (real.zerop() && imaginary.zerop()) {
//			return SingleFloatStruct.ONE;
//		}
//
//		final Apcomplex apcomplex = apcomplexValue();
//		final Apcomplex exp = ApcomplexMath.exp(apcomplex);
//		return makeComplexOrReal(exp);
		return null;
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
//		if (power.zerop()) {
//			if (power instanceof IntegerStruct) {
//				if (real instanceof FloatStruct) {
//					return ONE_FLOAT;
//				}
//				return IntegerStruct.ONE;
//			}
//			return ONE_FLOAT;
//		}
//		if (zerop() || isEqualTo(ONE)) {
//			return this;
//		}
//
//		final ExptVisitor<?> exptVisitor = exptVisitor();
//		return power.expt(exptVisitor);
		return null;
	}

	@Override
	public NumberStruct2 log() {
//		if (isEqualTo(ONE) || isEqualTo(ONE_FLOAT)) {
//			return FloatStruct2.ZERO;
//		}
//
//		final Apcomplex apcomplex = apcomplexValue();
//		final Apcomplex log = ApcomplexMath.log(apcomplex);
//		return makeComplexOrReal(log);
		return null;
	}

	@Override
	public NumberStruct2 log(final NumberStruct2 base) {
//		final Apcomplex apcomplex = apcomplexValue();
//		final Apcomplex baseVal = base.apcomplexValue();
//		final Apcomplex log = ApcomplexMath.log(apcomplex, baseVal);
//		return makeComplexOrReal(log);
		return null;
	}

	@Override
	public NumberStruct2 sqrt() {
//		if (real.zerop() && imaginary.zerop()) {
//			return IntegerStruct2.ZERO;
//		}
//
//		final Apcomplex apcomplex = apcomplexValue();
//		final Apcomplex sqrt = ApcomplexMath.sqrt(apcomplex);
//		return makeComplexOrReal(sqrt);
		return null;
	}

	@Override
	public NumberStruct2 sin() {
		final Apcomplex sin = ApcomplexMath.sin(apcomplex);
		return makeComplexOrReal(sin);
	}

	@Override
	public NumberStruct2 cos() {
		final Apcomplex cos = ApcomplexMath.cos(apcomplex);
		return makeComplexOrReal(cos);
	}

	@Override
	public NumberStruct2 tan() {
		final Apcomplex tan = ApcomplexMath.tan(apcomplex);
		return makeComplexOrReal(tan);
	}

	@Override
	public NumberStruct2 asin() {
		final Apcomplex asin = ApcomplexMath.asin(apcomplex);
		return makeComplexOrReal(asin);
	}

	@Override
	public NumberStruct2 acos() {
		final Apcomplex acos = ApcomplexMath.acos(apcomplex);
		return makeComplexOrReal(acos);
	}

	@Override
	public NumberStruct2 atan() {
		final Apcomplex atan = ApcomplexMath.atan(apcomplex);
		return makeComplexOrReal(atan);
	}

	@Override
	public NumberStruct2 sinh() {
		final Apcomplex sinh = ApcomplexMath.sinh(apcomplex);
		return makeComplexOrReal(sinh);
	}

	@Override
	public NumberStruct2 cosh() {
		final Apcomplex cosh = ApcomplexMath.cosh(apcomplex);
		return makeComplexOrReal(cosh);
	}

	@Override
	public NumberStruct2 tanh() {
		final Apcomplex tanh = ApcomplexMath.tanh(apcomplex);
		return makeComplexOrReal(tanh);
	}

	@Override
	public NumberStruct2 asinh() {
		final Apcomplex asinh = ApcomplexMath.asinh(apcomplex);
		return makeComplexOrReal(asinh);
	}

	@Override
	public NumberStruct2 acosh() {
		final Apcomplex acosh = ApcomplexMath.acosh(apcomplex);
		return makeComplexOrReal(acosh);
	}

	@Override
	public NumberStruct2 atanh() {
		final Apcomplex atanh = ApcomplexMath.atanh(apcomplex);
		return makeComplexOrReal(atanh);
	}

	public static NumberStruct2 makeComplexOrReal(final ComplexStruct2 complexStruct) {
//		if (complexStruct.imaginary.zerop()) {
//			return complexStruct.real;
//		}
//		return complexStruct;
		return null;
	}

	public static NumberStruct2 makeComplexOrReal(final RealStruct2 real, final RealStruct2 imaginary) {
//		if (imaginary.zerop()) {
//			return real;
//		}
//		return new ComplexStruct2(real, imaginary);
		return null;
	}

	public static NumberStruct2 makeComplexOrReal(final Apcomplex apcomplex) {
		final Apfloat real = apcomplex.real();
		final Apfloat imag = apcomplex.imag();
		if (Apcomplex.ZERO.equals(imag)) {
			return toRealStruct(real);
		}
		return new ComplexStruct2(apcomplex);
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(real)
		                            .append(imaginary)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ComplexStruct2 rhs = (ComplexStruct2) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(real, rhs.real)
		                          .append(imaginary, rhs.imaginary)
		                          .isEquals();
	}
}
