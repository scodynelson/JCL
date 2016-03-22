/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;

import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public interface RealStruct extends NumberStruct {

	BigDecimal bigDecimalValue();

	default Apfloat apfloatValue() {
		return new Apfloat(bigDecimalValue());
	}

	FloatStruct coerceRealToFloat();

	RealStruct zeroValue();

	boolean plusp();

	boolean minusp();

	@Override
	default NumberStruct.EqualToVisitor<?> equalToVisitor() {
		return new RealEqualToVisitor<>(this);
	}

	default boolean isLessThan(final RealStruct real) {
		final LessThanVisitor<?> lessThanVisitor = lessThanVisitor();
		return real.isLessThan(lessThanVisitor);
	}

	boolean isLessThan(LessThanVisitor<?> lessThanVisitor);

	static boolean isLessThan(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isLessThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default LessThanVisitor<?> lessThanVisitor() {
		return new LessThanVisitor<>(this);
	}

	default boolean isGreaterThan(final RealStruct real) {
		final GreaterThanVisitor<?> greaterThanVisitor = greaterThanVisitor();
		return real.isGreaterThan(greaterThanVisitor);
	}

	boolean isGreaterThan(GreaterThanVisitor<?> greaterThanVisitor);

	default GreaterThanVisitor<?> greaterThanVisitor() {
		return new GreaterThanVisitor<>(this);
	}

	static boolean isGreaterThan(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isGreaterThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default boolean isLessThanOrEqualTo(final RealStruct real) {
		final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor = lessThanOrEqualToVisitor();
		return real.isLessThanOrEqualTo(lessThanOrEqualToVisitor);
	}

	boolean isLessThanOrEqualTo(LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor);

	default LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new LessThanOrEqualToVisitor<>(this);
	}

	static boolean isLessThanOrEqualTo(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isLessThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default boolean isGreaterThanOrEqualTo(final RealStruct real) {
		final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor = greaterThanOrEqualToVisitor();
		return real.isGreaterThanOrEqualTo(greaterThanOrEqualToVisitor);
	}

	boolean isGreaterThanOrEqualTo(GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor);

	default GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new GreaterThanOrEqualToVisitor<>(this);
	}

	static boolean isGreaterThanOrEqualTo(final RealStruct real, final List<RealStruct> reals) {
		RealStruct previousReal = real;

		boolean result = true;
		for (final RealStruct currentReal : reals) {
			result = previousReal.isGreaterThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	default RealStruct max(final RealStruct real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	static RealStruct max(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::max);
	}

	default RealStruct min(final RealStruct real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	static RealStruct min(final RealStruct real, final List<RealStruct> reals) {
		if (reals.isEmpty()) {
			return real;
		}
		return reals.stream().reduce(real, RealStruct::min);
	}

	RealStruct rational();

	default RealStruct mod(final RealStruct divisor) {
		final QuotientRemainderResult floor = floor(divisor);
		return floor.getRemainder();
	}

	default RealStruct rem(final RealStruct divisor) {
		final QuotientRemainderResult truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	default ComplexStruct cis() {
		return new ComplexStruct(cos(), sin());
	}

	default QuotientRemainderResult floor() {
		return floor(IntegerStruct.ONE);
	}

	default QuotientRemainderResult floor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.floor(quotientRemainderVisitor);
	}

	QuotientRemainderResult floor(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult ffloor() {
		return ffloor(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ffloor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ffloor(quotientRemainderVisitor);
	}

	QuotientRemainderResult ffloor(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult ceiling() {
		return ceiling(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ceiling(quotientRemainderVisitor);
	}

	QuotientRemainderResult ceiling(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult fceiling() {
		return fceiling(IntegerStruct.ONE);
	}

	default QuotientRemainderResult fceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fceiling(quotientRemainderVisitor);
	}

	QuotientRemainderResult fceiling(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult truncate() {
		return truncate(IntegerStruct.ONE);
	}

	default QuotientRemainderResult truncate(final RealStruct divisor) {
		if ((plusp() && divisor.plusp()) || (minusp() && divisor.minusp())) {
			return floor(divisor);
		} else {
			return ceiling(divisor);
		}
	}

	default QuotientRemainderResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	default QuotientRemainderResult ftruncate(final RealStruct divisor) {
		if (plusp()) {
			return ffloor(divisor);
		} else {
			return fceiling(divisor);
		}
	}

	default QuotientRemainderResult round() {
		return round(IntegerStruct.ONE);
	}

	default QuotientRemainderResult round(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.round(quotientRemainderVisitor);
	}

	QuotientRemainderResult round(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	default QuotientRemainderResult fround() {
		return fround(IntegerStruct.ONE);
	}

	default QuotientRemainderResult fround(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fround(quotientRemainderVisitor);
	}

	QuotientRemainderResult fround(QuotientRemainderVisitor<?> quotientRemainderVisitor);

	QuotientRemainderVisitor<?> quotientRemainderVisitor();

	@Override
	default Apcomplex apcomplexValue() {
		return apfloatValue();
	}

	@Override
	default RealStruct realPart() {
		return this;
	}

	@Override
	default RealStruct conjugate() {
		return this;
	}

	@Override
	default RealStruct exp() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat exp = ApfloatMath.exp(apfloat);
		return new FloatStruct(exp);
	}

	@Override
	default NumberStruct.ExptVisitor<?> exptVisitor() {
		return new RealExptVisitor<>(this);
	}

	@Override
	default NumberStruct log() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex log = ApcomplexMath.log(apfloat);
			return ComplexStruct.makeComplexOrReal(log);
		}
		final Apfloat log = ApfloatMath.log(apfloat);
		return new FloatStruct(log);
	}

	@Override
	default NumberStruct log(final NumberStruct base) {
		final Apfloat apfloat = apfloatValue();
		if ((apfloat.signum() < 0) || (base instanceof ComplexStruct)) {
			final Apcomplex baseVal = base.apcomplexValue();
			final Apcomplex log = ApcomplexMath.log(apfloat, baseVal);
			return ComplexStruct.makeComplexOrReal(log);
		}
		final Apfloat baseVal = ((RealStruct) base).apfloatValue();
		final Apfloat log = ApfloatMath.log(apfloat, baseVal);
		return new FloatStruct(log);
	}

	@Override
	default NumberStruct sqrt() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex sqrt = ApcomplexMath.sqrt(apfloat);
			return ComplexStruct.makeComplexOrReal(sqrt);
		}
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		return new FloatStruct(sqrt);
	}

	@Override
	default RealStruct sin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sin = ApfloatMath.sin(apfloat);
		return new FloatStruct(sin);
	}

	@Override
	default RealStruct cos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cos = ApfloatMath.cos(apfloat);
		return new FloatStruct(cos);
	}

	@Override
	default RealStruct tan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tan = ApfloatMath.tan(apfloat);
		return new FloatStruct(tan);
	}

	@Override
	default RealStruct asin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asin = ApfloatMath.asin(apfloat);
		return new FloatStruct(asin);
	}

	@Override
	default RealStruct acos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acos = ApfloatMath.acos(apfloat);
		return new FloatStruct(acos);
	}

	@Override
	default RealStruct atan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atan = ApfloatMath.atan(apfloat);
		return new FloatStruct(atan);
	}

	default RealStruct atan(final RealStruct real) {
		final Apfloat apfloat1 = apfloatValue();
		final Apfloat apfloat2 = real.apfloatValue();

		final Apfloat atan = ApfloatMath.atan2(apfloat1, apfloat2);
		return new FloatStruct(atan);
	}

	@Override
	default RealStruct sinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sinh = ApfloatMath.sinh(apfloat);
		return new FloatStruct(sinh);
	}

	@Override
	default RealStruct cosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cosh = ApfloatMath.cosh(apfloat);
		return new FloatStruct(cosh);
	}

	@Override
	default RealStruct tanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tanh = ApfloatMath.tanh(apfloat);
		return new FloatStruct(tanh);
	}

	@Override
	default RealStruct asinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asinh = ApfloatMath.asinh(apfloat);
		return new FloatStruct(asinh);
	}

	@Override
	default RealStruct acosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acosh = ApfloatMath.acosh(apfloat);
		return new FloatStruct(acosh);
	}

	@Override
	default RealStruct atanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atanh = ApfloatMath.atanh(apfloat);
		return new FloatStruct(atanh);
	}

	static RealStruct toRealStruct(final Apfloat apfloat) {
		// TODO: Not quite right here either!!!
		return (apfloat.doubleValue() == apfloat.intValue()) ? new IntegerStruct(apfloat) : new FloatStruct(apfloat);
	}

	// Visitor Implementations

	abstract class RealAddVisitor<S extends RealStruct> extends NumberStruct.AddVisitor<S> {

		RealAddVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct add(IntegerStruct number2);

		@Override
		public RealStruct add(final FloatStruct number2) {
			return addFloat(number2, number1);
		}

		@Override
		public abstract RealStruct add(RatioStruct number2);

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex add = apfloat1.add(apcomplex2);
			return ComplexStruct.makeComplexOrReal(add);
		}

		/**
		 * Computes the addition for the provided {@link FloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#add(BigDecimal)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link FloatStruct} as the first argument of the addition operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the addition operation
		 *
		 * @return a new {@link FloatStruct} as the result of the addition operation
		 */
		static RealStruct addFloat(final FloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return new FloatStruct(add);
		}
	}

	abstract class RealSubtractVisitor<S extends RealStruct> extends NumberStruct.SubtractVisitor<S> {

		RealSubtractVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct subtract(IntegerStruct number2);

		@Override
		public RealStruct subtract(final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}

		@Override
		public abstract RealStruct subtract(RatioStruct number2);

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex subtract = apfloat1.subtract(apcomplex2);
			return ComplexStruct.makeComplexOrReal(subtract);
		}
	}

	abstract class RealMultiplyVisitor<S extends RealStruct> extends NumberStruct.MultiplyVisitor<S> {

		RealMultiplyVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct multiply(IntegerStruct number2);

		@Override
		public RealStruct multiply(final FloatStruct number2) {
			return multiplyFloat(number2, number1);
		}

		@Override
		public abstract RealStruct multiply(RatioStruct number2);

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex multiply = apfloat1.multiply(apcomplex2);
			return ComplexStruct.makeComplexOrReal(multiply);
		}

		/**
		 * Computes the multiplication for the provided {@link FloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#multiply(BigDecimal)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link FloatStruct} as the first argument of the multiplication operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the multiplication operation
		 *
		 * @return a new {@link FloatStruct} as the result of the multiplication operation
		 */
		static RealStruct multiplyFloat(final FloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);

			// NOTE: We must both strip the trailing zeros and possibly reset the scale to 1 if there were only trailing zeros
			BigDecimal preppedBigDecimal = multiply.stripTrailingZeros();
			if (preppedBigDecimal.scale() == 0) {
				preppedBigDecimal = preppedBigDecimal.setScale(1, RoundingMode.UNNECESSARY);
			}

			return new FloatStruct(preppedBigDecimal);
		}
	}

	abstract class RealDivideVisitor<S extends RealStruct> extends NumberStruct.DivideVisitor<S> {

		RealDivideVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct divide(IntegerStruct number2);

		@Override
		public RealStruct divide(final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}

		@Override
		public abstract RealStruct divide(RatioStruct number2);

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex divide = apfloat1.divide(apcomplex2);
			return ComplexStruct.makeComplexOrReal(divide);
		}
	}

	static int getComparisonResult(final RealStruct real1, final RealStruct real2) {
		final BigDecimal bigDecimal1 = real1.bigDecimalValue();
		final BigDecimal bigDecimal2 = real2.bigDecimalValue();
		return bigDecimal1.compareTo(bigDecimal2);
	}

	class RealEqualToVisitor<S extends RealStruct> extends NumberStruct.EqualToVisitor<S> {

		RealEqualToVisitor(final S number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link FloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public boolean equalTo(final IntegerStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final FloatStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the numeric '=' equality result for an {@link FloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public boolean equalTo(final RatioStruct number2) {
			return getComparisonResult(number1, number2) == 0;
		}

		@Override
		public boolean equalTo(final ComplexStruct number2) {
			return number2.isEqualTo(number1);
		}
	}

	class LessThanVisitor<S extends RealStruct> {

		final S real1;

		LessThanVisitor(final S real1) {
			this.real1 = real1;
		}

		public boolean lessThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		public boolean lessThan(final FloatStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}

		public boolean lessThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) < 0;
		}
	}

	class GreaterThanVisitor<S extends RealStruct> {

		final S real1;

		GreaterThanVisitor(final S real1) {
			this.real1 = real1;
		}

		public boolean greaterThan(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		public boolean greaterThan(final FloatStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}

		public boolean greaterThan(final RatioStruct real2) {
			return getComparisonResult(real1, real2) > 0;
		}
	}

	class LessThanOrEqualToVisitor<S extends RealStruct> {

		final S real1;

		LessThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		public boolean lessThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		public boolean lessThanOrEqualTo(final FloatStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}

		public boolean lessThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) <= 0;
		}
	}

	class GreaterThanOrEqualToVisitor<S extends RealStruct> {

		final S real1;

		GreaterThanOrEqualToVisitor(final S real1) {
			this.real1 = real1;
		}

		public boolean greaterThanOrEqualTo(final IntegerStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		public boolean greaterThanOrEqualTo(final FloatStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}

		public boolean greaterThanOrEqualTo(final RatioStruct real2) {
			return getComparisonResult(real1, real2) >= 0;
		}
	}

	abstract class QuotientRemainderVisitor<S extends RealStruct> {

		final S real;

		QuotientRemainderVisitor(final S real) {
			this.real = real;
		}

		public QuotientRemainderResult floor(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult floor(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult floor(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult ffloor(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ffloor(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ffloor(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ceiling(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult ceiling(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult ceiling(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult fceiling(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult fceiling(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult fceiling(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult round(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult round(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult round(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult fround(final IntegerStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		public QuotientRemainderResult fround(final FloatStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		public QuotientRemainderResult fround(final RatioStruct divisor) {
			return quotientRemainder(divisor, RoundingMode.HALF_EVEN, true);
		}

		public abstract QuotientRemainderResult quotientRemainder(IntegerStruct divisor, RoundingMode roundingMode,
		                                                          boolean isQuotientFloat);

		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		QuotientRemainderResult floatQuotientRemainder(final RealStruct divisor, final RoundingMode roundingMode,
		                                               final boolean isQuotientFloat) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = getFloatQuotient(divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = new IntegerStruct(quotientBigInteger);
			}

			final FloatStruct remainderFloat = new FloatStruct(remainder);
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		private RealStruct getFloatQuotient(final RealStruct divisor, final BigDecimal quotient) {
			final RealStruct floatQuotient;
			if (BigDecimal.ZERO.compareTo(quotient) == 0) {
				if (real.minusp()) {
					if (divisor.minusp()) {
						floatQuotient = FloatStruct.ZERO;
					} else {
						floatQuotient = FloatStruct.MINUS_ZERO;
					}
				} else if (divisor.minusp()) {
					floatQuotient = FloatStruct.MINUS_ZERO;
				} else {
					floatQuotient = FloatStruct.ZERO;
				}
			} else {
				floatQuotient = new FloatStruct(quotient);
			}
			return floatQuotient;
		}


		public abstract QuotientRemainderResult quotientRemainder(RatioStruct divisor, RoundingMode roundingMode,
		                                                          boolean isQuotientFloat);
	}

	class RealExptVisitor<S extends RealStruct> extends NumberStruct.ExptVisitor<S> {

		RealExptVisitor(final S base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntegerStruct power) {
			return exptInteger(base, power);
		}

		@Override
		public NumberStruct expt(final FloatStruct power) {
			return exptFloatRatio(base, power);
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			return exptFloatRatio(base, power);
		}

		private static NumberStruct exptFloatRatio(final RealStruct base, final RealStruct power) {
			final double x = base.apfloatValue().doubleValue();
			final double y = power.apfloatValue().doubleValue();

			double result = StrictMath.pow(x, y);
			if (Double.isNaN(result)) {
				if (x < 0) {
					result = StrictMath.pow(-x, y);
					final double realPart = result * StrictMath.cos(y * Math.PI);
					final double imagPart = result * StrictMath.sin(y * Math.PI);

					final BigDecimal realBigDecimal = BigDecimal.valueOf(realPart);
					final FloatStruct real = new FloatStruct(realBigDecimal);
					final BigDecimal imagBigDecimal = BigDecimal.valueOf(imagPart);
					final FloatStruct imaginary = new FloatStruct(imagBigDecimal);
					return ComplexStruct.makeComplexOrReal(real, imaginary);
				}
			}
			final BigDecimal resultBigDecimal = BigDecimal.valueOf(result);
			return new FloatStruct(resultBigDecimal);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			final RealStruct powerComplexReal = power.getReal();
			final BigDecimal powerComplexRealBigDecimal = powerComplexReal.bigDecimalValue();
			final FloatStruct real = new FloatStruct(powerComplexRealBigDecimal);

			final RealStruct powerComplexImaginary = power.getImaginary();
			final BigDecimal powerComplexImaginaryBigDecimal = powerComplexImaginary.bigDecimalValue();
			final FloatStruct imaginary = new FloatStruct(powerComplexImaginaryBigDecimal);

			final NumberStruct newPowerComplex = ComplexStruct.makeComplexOrReal(real, imaginary);

			final BigDecimal bigDecimal1 = base.bigDecimalValue();
			final RealStruct newBase = new FloatStruct(bigDecimal1);
			final NumberStruct logOfNewBase = newBase.log();
			final NumberStruct powerComplexLogOfNewBaseProduct = newPowerComplex.multiply(logOfNewBase);
			return powerComplexLogOfNewBaseProduct.exp();
		}
	}
}
