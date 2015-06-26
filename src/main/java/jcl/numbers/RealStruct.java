/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.types.RealType;
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public abstract class RealStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7159935653316309907L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(RealType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the real object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final RealType type,
	                     final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	public abstract BigDecimal bigDecimalValue();

	public Apfloat apfloatValue() {
		return new Apfloat(bigDecimalValue());
	}

	public abstract FloatStruct coerceRealToFloat();

	public abstract RealStruct zeroValue();

	public abstract boolean plusp();

	public abstract boolean minusp();

	@Override
	protected EqualToVisitor<?> equalToVisitor() {
		return new RealEqualToVisitor<>(this);
	}

	public boolean isLessThan(final RealStruct real) {
		final LessThanVisitor<?> lessThanVisitor = lessThanVisitor();
		return real.isLessThan(lessThanVisitor);
	}

	protected abstract boolean isLessThan(final LessThanVisitor<?> lessThanVisitor);

	public static boolean isLessThan(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to test equality.");
		}

		RealStruct previousReal = reals[0];

		boolean result = true;
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = previousReal.isLessThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	protected LessThanVisitor<?> lessThanVisitor() {
		return new LessThanVisitor<>(this);
	}

	public boolean isGreaterThan(final RealStruct real) {
		final GreaterThanVisitor<?> greaterThanVisitor = greaterThanVisitor();
		return real.isGreaterThan(greaterThanVisitor);
	}

	protected abstract boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor);

	protected GreaterThanVisitor<?> greaterThanVisitor() {
		return new GreaterThanVisitor<>(this);
	}

	public static boolean isGreaterThan(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to test equality.");
		}

		RealStruct previousReal = reals[0];

		boolean result = true;
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = previousReal.isGreaterThan(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	public boolean isLessThanOrEqualTo(final RealStruct real) {
		final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor = lessThanOrEqualToVisitor();
		return real.isLessThanOrEqualTo(lessThanOrEqualToVisitor);
	}

	protected abstract boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor);

	protected LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new LessThanOrEqualToVisitor<>(this);
	}

	public static boolean isLessThanOrEqualTo(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to test equality.");
		}

		RealStruct previousReal = reals[0];

		boolean result = true;
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = previousReal.isLessThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor = greaterThanOrEqualToVisitor();
		return real.isGreaterThanOrEqualTo(greaterThanOrEqualToVisitor);
	}

	protected abstract boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor);

	protected GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new GreaterThanOrEqualToVisitor<>(this);
	}

	public static boolean isGreaterThanOrEqualTo(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to test equality.");
		}

		RealStruct previousReal = reals[0];

		boolean result = true;
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = previousReal.isGreaterThanOrEqualTo(currentReal);
			if (!result) {
				break;
			}
			previousReal = currentReal;
		}
		return result;
	}

	public RealStruct max(final RealStruct real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	public static RealStruct max(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to find max.");
		}

		RealStruct result = reals[0];
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = result.max(currentReal);
		}
		return result;
	}

	public RealStruct min(final RealStruct real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	public static RealStruct min(final RealStruct... reals) {
		if (reals.length == 0) {
			throw new ErrorException("At least one real required to find min.");
		}
		if (reals.length == 1) {
			return reals[0];
		}

		RealStruct result = reals[0];
		for (int i = 1; i < reals.length; i++) {
			final RealStruct currentReal = reals[i];
			result = result.min(currentReal);
		}
		return result;
	}

	public abstract RealStruct rational();

	public RealStruct mod(final RealStruct divisor) {
		final QuotientRemainderResult floor = floor(divisor);
		return floor.getRemainder();
	}

	public RealStruct rem(final RealStruct divisor) {
		final QuotientRemainderResult truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	public ComplexStruct cis() {
		return new ComplexStruct(cos(), sin());
	}

	public QuotientRemainderResult floor() {
		return floor(IntegerStruct.ONE);
	}

	public QuotientRemainderResult floor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.floor(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	public QuotientRemainderResult ffloor() {
		return ffloor(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ffloor(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ffloor(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	public QuotientRemainderResult ceiling() {
		return ceiling(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.ceiling(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	public QuotientRemainderResult fceiling() {
		return fceiling(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fceiling(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	public QuotientRemainderResult truncate() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult truncate(final RealStruct divisor) {
		if ((plusp() && divisor.plusp()) || (minusp() && divisor.minusp())) {
			return floor(divisor);
		} else {
			return ceiling(divisor);
		}
	}

	public QuotientRemainderResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ftruncate(final RealStruct divisor) {
		if (plusp()) {
			return ffloor(divisor);
		} else {
			return fceiling(divisor);
		}
	}

	public QuotientRemainderResult round() {
		return round(IntegerStruct.ONE);
	}

	public QuotientRemainderResult round(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.round(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	public QuotientRemainderResult fround() {
		return fround(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fround(final RealStruct divisor) {
		final QuotientRemainderVisitor<?> quotientRemainderVisitor = quotientRemainderVisitor();
		return divisor.fround(quotientRemainderVisitor);
	}

	protected abstract QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor);

	protected abstract QuotientRemainderVisitor<?> quotientRemainderVisitor();

	@Override
	public Apcomplex apcomplexValue() {
		return apfloatValue();
	}

	@Override
	public RealStruct realPart() {
		return this;
	}

	@Override
	public RealStruct conjugate() {
		return this;
	}

	@Override
	public RealStruct exp() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat exp = ApfloatMath.exp(apfloat);
		return new FloatStruct(exp);
	}

	@Override
	protected ExptVisitor<?> exptVisitor() {
		return new RealExptVisitor<>(this);
	}

	@Override
	public NumberStruct log() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex log = ApcomplexMath.log(apfloat);
			return ComplexStruct.makeComplexOrReal(log);
		}
		final Apfloat log = ApfloatMath.log(apfloat);
		return new FloatStruct(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
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
	public NumberStruct sqrt() {
		final Apfloat apfloat = apfloatValue();
		if (apfloat.signum() < 0) {
			final Apcomplex sqrt = ApcomplexMath.sqrt(apfloat);
			return ComplexStruct.makeComplexOrReal(sqrt);
		}
		final Apfloat sqrt = ApfloatMath.sqrt(apfloat);
		return new FloatStruct(sqrt);
	}

	@Override
	public RealStruct sin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sin = ApfloatMath.sin(apfloat);
		return new FloatStruct(sin);
	}

	@Override
	public RealStruct cos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cos = ApfloatMath.cos(apfloat);
		return new FloatStruct(cos);
	}

	@Override
	public RealStruct tan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tan = ApfloatMath.tan(apfloat);
		return new FloatStruct(tan);
	}

	@Override
	public RealStruct asin() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asin = ApfloatMath.asin(apfloat);
		return new FloatStruct(asin);
	}

	@Override
	public RealStruct acos() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acos = ApfloatMath.acos(apfloat);
		return new FloatStruct(acos);
	}

	@Override
	public RealStruct atan() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atan = ApfloatMath.atan(apfloat);
		return new FloatStruct(atan);
	}

	public RealStruct atan(final RealStruct real) {
		final Apfloat apfloat1 = apfloatValue();
		final Apfloat apfloat2 = real.apfloatValue();

		final Apfloat atan = ApfloatMath.atan2(apfloat1, apfloat2);
		return new FloatStruct(atan);
	}

	@Override
	public RealStruct sinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat sinh = ApfloatMath.sinh(apfloat);
		return new FloatStruct(sinh);
	}

	@Override
	public RealStruct cosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat cosh = ApfloatMath.cosh(apfloat);
		return new FloatStruct(cosh);
	}

	@Override
	public RealStruct tanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat tanh = ApfloatMath.tanh(apfloat);
		return new FloatStruct(tanh);
	}

	@Override
	public RealStruct asinh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat asinh = ApfloatMath.asinh(apfloat);
		return new FloatStruct(asinh);
	}

	@Override
	public RealStruct acosh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat acosh = ApfloatMath.acosh(apfloat);
		return new FloatStruct(acosh);
	}

	@Override
	public RealStruct atanh() {
		final Apfloat apfloat = apfloatValue();
		final Apfloat atanh = ApfloatMath.atanh(apfloat);
		return new FloatStruct(atanh);
	}

	public static RealStruct toRealStruct(final Apfloat apfloat) {
		return (apfloat instanceof Apint) ? new IntegerStruct(apfloat) : new FloatStruct(apfloat);
	}

	// Visitor Implementations

	protected abstract static class RealAddVisitor<S extends RealStruct> extends AddVisitor<S> {

		protected RealAddVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct add(final IntegerStruct number2);

		@Override
		public RealStruct add(final FloatStruct number2) {
			return addFloat(number2, number1);
		}

		@Override
		public abstract RealStruct add(final RatioStruct number2);

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
		protected static RealStruct addFloat(final FloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return new FloatStruct(add);
		}
	}

	protected abstract static class RealSubtractVisitor<S extends RealStruct> extends SubtractVisitor<S> {

		protected RealSubtractVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct subtract(final IntegerStruct number2);

		@Override
		public RealStruct subtract(final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}

		@Override
		public abstract RealStruct subtract(final RatioStruct number2);

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex subtract = apfloat1.subtract(apcomplex2);
			return ComplexStruct.makeComplexOrReal(subtract);
		}
	}

	protected abstract static class RealMultiplyVisitor<S extends RealStruct> extends MultiplyVisitor<S> {

		protected RealMultiplyVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct multiply(final IntegerStruct number2);

		@Override
		public RealStruct multiply(final FloatStruct number2) {
			return multiplyFloat(number2, number1);
		}

		@Override
		public abstract RealStruct multiply(final RatioStruct number2);

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
		protected static RealStruct multiplyFloat(final FloatStruct number1, final RealStruct number2) {
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

	protected abstract static class RealDivideVisitor<S extends RealStruct> extends DivideVisitor<S> {

		protected RealDivideVisitor(final S number1) {
			super(number1);
		}

		@Override
		public abstract RealStruct divide(final IntegerStruct number2);

		@Override
		public RealStruct divide(final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}

		@Override
		public abstract RealStruct divide(final RatioStruct number2);

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			final Apcomplex apfloat1 = number1.apfloatValue();
			final Apcomplex apcomplex2 = number2.apcomplexValue();

			final Apcomplex divide = apfloat1.divide(apcomplex2);
			return ComplexStruct.makeComplexOrReal(divide);
		}
	}

	protected static int getComparisonResult(final RealStruct real1, final RealStruct real2) {
		final BigDecimal bigDecimal1 = real1.bigDecimalValue();
		final BigDecimal bigDecimal2 = real2.bigDecimalValue();
		return bigDecimal1.compareTo(bigDecimal2);
	}

	protected static class RealEqualToVisitor<S extends RealStruct> extends EqualToVisitor<S> {

		protected RealEqualToVisitor(final S number1) {
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

	protected static class LessThanVisitor<S extends RealStruct> {

		protected final S real1;

		protected LessThanVisitor(final S real1) {
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

	protected static class GreaterThanVisitor<S extends RealStruct> {

		protected final S real1;

		protected GreaterThanVisitor(final S real1) {
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

	protected static class LessThanOrEqualToVisitor<S extends RealStruct> {

		protected final S real1;

		protected LessThanOrEqualToVisitor(final S real1) {
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

	protected static class GreaterThanOrEqualToVisitor<S extends RealStruct> {

		protected final S real1;

		protected GreaterThanOrEqualToVisitor(final S real1) {
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

	protected abstract static class QuotientRemainderVisitor<S extends RealStruct> {

		protected final S real;

		protected QuotientRemainderVisitor(final S real) {
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

		public abstract QuotientRemainderResult quotientRemainder(final IntegerStruct divisor, final RoundingMode roundingMode,
		                                                          final boolean isQuotientFloat);

		public QuotientRemainderResult quotientRemainder(final FloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(divisor, roundingMode, isQuotientFloat);
		}

		protected QuotientRemainderResult floatQuotientRemainder(final RealStruct divisor, final RoundingMode roundingMode,
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


		public abstract QuotientRemainderResult quotientRemainder(final RatioStruct divisor, final RoundingMode roundingMode,
		                                                          final boolean isQuotientFloat);
	}

	protected static class RealExptVisitor<S extends RealStruct> extends ExptVisitor<S> {

		protected RealExptVisitor(final S base) {
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

			double result = FastMath.pow(x, y);
			if (Double.isNaN(result)) {
				if (x < 0) {
					result = FastMath.pow(-x, y);
					final double realPart = result * FastMath.cos(y * Math.PI);
					final double imagPart = result * FastMath.sin(y * Math.PI);

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
