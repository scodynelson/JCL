/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.classes.BuiltInClassStruct;
import jcl.types.ComplexType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends BuiltInClassStruct implements NumberStruct {

	/**
	 * {@link ComplexStruct} constant representing I.
	 */
	public static final ComplexStruct I = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);

	/**
	 * {@link ComplexStruct} constant representing -I.
	 */
	public static final ComplexStruct NEGATE_I = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE);

	/**
	 * {@link ComplexStruct} constant representing 0.
	 */
	public static final ComplexStruct ZERO = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 0.0.
	 */
	public static final ComplexStruct ZERO_FLOAT = new ComplexStruct(SingleFloatStruct.ZERO, SingleFloatStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 1.
	 */
	public static final ComplexStruct ONE = new ComplexStruct(IntegerStruct.ONE, IntegerStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 1.0.
	 */
	public static final ComplexStruct ONE_FLOAT = new ComplexStruct(SingleFloatStruct.ONE, SingleFloatStruct.ZERO);

	/**
	 * The {@link RealStruct} that comprises the real value of the complex.
	 */
	private final RealStruct real;

	/**
	 * The {@link RealStruct} that comprises the imaginary value of the complex.
	 */
	private final RealStruct imaginary;

	//	/**
//	 * Public constructor.
//	 *
//	 * @param real
//	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
//	 * @param imaginary
//	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
//	 */
	ComplexStruct(final Apcomplex apcomplex) {
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
	ComplexStruct(final Apfloat real, final Apfloat imaginary) {
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
	public ComplexStruct(final RealStruct real, final RealStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);

		RealStruct coercedReal = real;
		RealStruct coercedImaginary = imaginary;
		if ((real instanceof FloatStruct) || (imaginary instanceof FloatStruct)) {
			coercedReal = real.floatingPoint();
			coercedImaginary = imaginary.floatingPoint();
		}
		this.real = coercedReal;
		this.imaginary = coercedImaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real.floatingPoint();
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary.floatingPoint();
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary.floatingPoint();
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real.floatingPoint();
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	private static RealStruct toRealStruct(final Apfloat apfloat) {
		// TODO: Not quite right here either!!!
		return (apfloat.doubleValue() == apfloat.intValue()) ? IntegerStruct.valueOf(apfloat) : FloatStruct.valueOf(apfloat);
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getReal() {
		return real;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getImaginary() {
		return imaginary;
	}

	@Override
	public Apcomplex apcomplexValue() {
		final Apfloat apfloatReal = real.apfloatValue();
		final Apfloat apfloatImag = imaginary.apfloatValue();
		return new Apcomplex(apfloatReal, apfloatImag);
	}

	@Override
	public RealStruct abs() {
		if (real.zerop()) {
			return imaginary.abs();
		}

		final Apcomplex apcomplex = apcomplexValue();
		final Apfloat abs = ApcomplexMath.abs(apcomplex);
		return toRealStruct(abs);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this ComplexStruct is zero using {@link RealStruct#zerop} on {@link #real} and {@link
	 * #imaginary}.
	 */
	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	@Override
	public NumberStruct add(final NumberStruct.AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	public NumberStruct.AddVisitor<?> addVisitor() {
		return new ComplexAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final NumberStruct.SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public NumberStruct.SubtractVisitor<?> subtractVisitor() {
		return new ComplexSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final NumberStruct.MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public NumberStruct.MultiplyVisitor<?> multiplyVisitor() {
		return new ComplexMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final NumberStruct.DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public NumberStruct.DivideVisitor<?> divideVisitor() {
		return new ComplexDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final NumberStruct.EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public NumberStruct.EqualToVisitor<?> equalToVisitor() {
		return new ComplexEqualToVisitor(this);
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		}

		final RealStruct abs = abs();
		return divide(abs);
	}

	@Override
	public RealStruct realPart() {
		return real;
	}

	@Override
	public RealStruct imagPart() {
		return imaginary;
	}

	@Override
	public NumberStruct conjugate() {
		final NumberStruct imagNegation = imaginary.negation();
		return makeComplexOrReal(real, (RealStruct) imagNegation);
	}

	@Override
	public NumberStruct negation() {
		return makeComplexOrReal((RealStruct) real.negation(), (RealStruct) imaginary.negation());
	}

	@Override
	public NumberStruct reciprocal() {
		return ONE.divide(this);
	}

	@Override
	public NumberStruct exp() {
		if (real.zerop() && imaginary.zerop()) {
			return SingleFloatStruct.ONE;
		}

		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex exp = ApcomplexMath.exp(apcomplex);
		return makeComplexOrReal(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				if (real instanceof FloatStruct) {
					return ONE_FLOAT;
				}
				return IntegerStruct.ONE;
			}
			return ONE_FLOAT;
		}
		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	@Override
	public NumberStruct expt(final NumberStruct.ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public NumberStruct.ExptVisitor<?> exptVisitor() {
		return new ComplexExptVisitor(this);
	}

	@Override
	public NumberStruct log() {
		if (isEqualTo(ONE) || isEqualTo(ONE_FLOAT)) {
			return SingleFloatStruct.ZERO;
		}

		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex log = ApcomplexMath.log(apcomplex);
		return makeComplexOrReal(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex baseVal = base.apcomplexValue();
		final Apcomplex log = ApcomplexMath.log(apcomplex, baseVal);
		return makeComplexOrReal(log);
	}

	@Override
	public NumberStruct sqrt() {
		if (real.zerop() && imaginary.zerop()) {
			return IntegerStruct.ZERO;
		}

		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex sqrt = ApcomplexMath.sqrt(apcomplex);
		return makeComplexOrReal(sqrt);
	}

	@Override
	public NumberStruct sin() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex sin = ApcomplexMath.sin(apcomplex);
		return makeComplexOrReal(sin);
	}

	@Override
	public NumberStruct cos() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex cos = ApcomplexMath.cos(apcomplex);
		return makeComplexOrReal(cos);
	}

	@Override
	public NumberStruct tan() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex tan = ApcomplexMath.tan(apcomplex);
		return makeComplexOrReal(tan);
	}

	@Override
	public NumberStruct asin() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex asin = ApcomplexMath.asin(apcomplex);
		return makeComplexOrReal(asin);
	}

	@Override
	public NumberStruct acos() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex acos = ApcomplexMath.acos(apcomplex);
		return makeComplexOrReal(acos);
	}

	@Override
	public NumberStruct atan() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex atan = ApcomplexMath.atan(apcomplex);
		return makeComplexOrReal(atan);
	}

	@Override
	public NumberStruct sinh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex sinh = ApcomplexMath.sinh(apcomplex);
		return makeComplexOrReal(sinh);
	}

	@Override
	public NumberStruct cosh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex cosh = ApcomplexMath.cosh(apcomplex);
		return makeComplexOrReal(cosh);
	}

	@Override
	public NumberStruct tanh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex tanh = ApcomplexMath.tanh(apcomplex);
		return makeComplexOrReal(tanh);
	}

	@Override
	public NumberStruct asinh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex asinh = ApcomplexMath.asinh(apcomplex);
		return makeComplexOrReal(asinh);
	}

	@Override
	public NumberStruct acosh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex acosh = ApcomplexMath.acosh(apcomplex);
		return makeComplexOrReal(acosh);
	}

	@Override
	public NumberStruct atanh() {
		final Apcomplex apcomplex = apcomplexValue();
		final Apcomplex atanh = ApcomplexMath.atanh(apcomplex);
		return makeComplexOrReal(atanh);
	}

	public static NumberStruct makeComplexOrReal(final ComplexStruct complexStruct) {
		if (complexStruct.imaginary.zerop()) {
			return complexStruct.real;
		}
		return complexStruct;
	}

	public static NumberStruct makeComplexOrReal(final RealStruct real, final RealStruct imaginary) {
		if (imaginary.zerop()) {
			return real;
		}
		return new ComplexStruct(real, imaginary);
	}

	public static NumberStruct makeComplexOrReal(final Apcomplex apcomplex) {
		final Apfloat real = apcomplex.real();
		final Apfloat imag = apcomplex.imag();
		if (Apcomplex.ZERO.equals(imag)) {
			return toRealStruct(real);
		}
		return new ComplexStruct(apcomplex);
	}

	// Visitor Implementations

	/**
	 * {@link NumberStruct.AddVisitor} for computing addition results for {@link ComplexStruct}s.
	 */
	private static final class ComplexAddVisitor extends NumberStruct.AddVisitor<ComplexStruct> {

		private ComplexAddVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public NumberStruct add(final IntIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final LongIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final BigIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final SingleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final DoubleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final RatioStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex add = apcomplex1.add(apfloat2);
			return makeComplexOrReal(add);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex add = apcomplex1.add(apcomplex2);
			return makeComplexOrReal(add);
		}
	}

	/**
	 * {@link NumberStruct.SubtractVisitor} for computing subtraction function results for {@link ComplexStruct}s.
	 */
	private static final class ComplexSubtractVisitor extends NumberStruct.SubtractVisitor<ComplexStruct> {

		private ComplexSubtractVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public NumberStruct subtract(final IntIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final LongIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final BigIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final SingleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final DoubleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final RatioStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex subtract = apcomplex1.subtract(apfloat2);
			return makeComplexOrReal(subtract);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex subtract = apcomplex1.subtract(apcomplex2);
			return makeComplexOrReal(subtract);
		}
	}

	/**
	 * {@link NumberStruct.MultiplyVisitor} for computing multiplication function results for {@link ComplexStruct}s.
	 */
	private static final class ComplexMultiplyVisitor extends NumberStruct.MultiplyVisitor<ComplexStruct> {

		private ComplexMultiplyVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public NumberStruct multiply(final IntIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final LongIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final BigIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final SingleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final DoubleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final RatioStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex multiply = apcomplex1.multiply(apfloat2);
			return makeComplexOrReal(multiply);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex multiply = apcomplex1.multiply(apcomplex2);
			return makeComplexOrReal(multiply);
		}
	}

	/**
	 * {@link NumberStruct.DivideVisitor} for computing division function results for {@link ComplexStruct}s.
	 */
	private static final class ComplexDivideVisitor extends NumberStruct.DivideVisitor<ComplexStruct> {

		private ComplexDivideVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public NumberStruct divide(final IntIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final LongIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final BigIntegerStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final SingleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final DoubleFloatStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final RatioStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apfloat2 = number2.apfloatValue();

			final Apcomplex divide = apcomplex1.divide(apfloat2);
			return makeComplexOrReal(divide);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			final Apcomplex apcomplex1 = number1.apcomplexValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex divide = apcomplex1.divide(apcomplex2);
			return makeComplexOrReal(divide);
		}
	}

	/**
	 * {@link NumberStruct.EqualToVisitor} for computing numeric '=' equality results for {@link ComplexStruct}s.
	 */
	private static final class ComplexEqualToVisitor extends NumberStruct.EqualToVisitor<ComplexStruct> {

		private ComplexEqualToVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final IntIntegerStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final LongIntegerStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final BigIntegerStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final RatioStruct number2) {
			return equalToReal(number1, number2);
		}

		/**
		 * Tests the equality of the provided {@link ComplexStruct} and {@link RealStruct}. A {@link RealStruct} and a
		 * {@link ComplexStruct} are only equal if the {@link ComplexStruct#imaginary} is equivalent to '0' and the
		 * {@link ComplexStruct#real} is equal to the provided {@link RealStruct}.
		 *
		 * @param complex
		 * 		the {@link ComplexStruct} to test for equality to the provided {@link RealStruct}
		 * @param real
		 * 		the {@link RealStruct} to test for equality to the provided {@link ComplexStruct}
		 *
		 * @return true if the number are equivalent; false otherwise
		 */
		private static boolean equalToReal(final ComplexStruct complex, final RealStruct real) {
			final RealStruct realVal1 = complex.getReal();
			final RealStruct imaginaryVal1 = complex.getImaginary();
			return imaginaryVal1.zerop() && realVal1.isEqualTo(real);
		}

		@Override
		public boolean equalTo(final ComplexStruct number2) {
			final RealStruct realVal1 = number1.getReal();
			final RealStruct imaginaryVal1 = number1.getImaginary();

			final RealStruct realVal2 = number2.getReal();
			final RealStruct imaginaryVal2 = number2.getImaginary();

			return realVal1.isEqualTo(realVal2) && imaginaryVal1.isEqualTo(imaginaryVal2);
		}
	}

	/**
	 * {@link NumberStruct.ExptVisitor} for computing exponential function results for {@link ComplexStruct}s.
	 */
	private static final class ComplexExptVisitor extends NumberStruct.ExptVisitor<ComplexStruct> {

		private ComplexExptVisitor(final ComplexStruct number1) {
			super(number1);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			if (base.getReal() instanceof RationalStruct) {
				return exptInteger(base, power);
			}
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerComplexLogOfBaseProduct = power.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			if (base.getReal() instanceof RationalStruct) {
				return exptInteger(base, power);
			}
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerComplexLogOfBaseProduct = power.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			if (base.getReal() instanceof RationalStruct) {
				return exptInteger(base, power);
			}
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerComplexLogOfBaseProduct = power.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			return exptComplex(base, power);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			return exptComplex(base, power);
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			return exptComplex(base, power);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			return exptComplex(base, power);
		}

		/**
		 * Determines the exponential result from applying the provided {@link NumberStruct} power to the provided
		 * {@link ComplexStruct} base value.
		 *
		 * @param base
		 * 		the {@link ComplexStruct} to be raised to the provided {@link NumberStruct} power
		 * @param power
		 * 		the {@link NumberStruct} power to raise the provided {@link ComplexStruct} base
		 *
		 * @return exponential result from applying the provided {@link NumberStruct} power to the provided {@link
		 * ComplexStruct} base value
		 */
		private static NumberStruct exptComplex(final ComplexStruct base, final NumberStruct power) {
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerLogOfBaseProduct = power.multiply(logOfBase);
			return powerLogOfBaseProduct.exp();
		}
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
		final ComplexStruct rhs = (ComplexStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(real, rhs.real)
		                          .append(imaginary, rhs.imaginary)
		                          .isEquals();
	}
}
