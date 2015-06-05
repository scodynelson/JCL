/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;
import java.util.Objects;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.types.RealType;
import org.apache.commons.math3.util.FastMath;

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

	public abstract double doubleValue();

	public abstract BigDecimal bigDecimalValue();

	public abstract FloatStruct coerceRealToFloat();

	public abstract RealStruct zeroValue();

	public abstract boolean plusp();

	public abstract boolean minusp();

	public abstract boolean isLessThan(final RealStruct real);

	public abstract boolean isGreaterThan(final RealStruct real);

	public abstract boolean isLessThanOrEqualTo(final RealStruct real);

	public abstract boolean isGreaterThanOrEqualTo(final RealStruct real);

	public RealStruct max(final RealStruct real) {
		return MaxStrategy.INSTANCE.max(this, real);
	}

	public RealStruct min(final RealStruct real) {
		return MinStrategy.INSTANCE.min(this, real);
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

	public abstract QuotientRemainderResult floor(final RealStruct divisor);

	public QuotientRemainderResult ffloor() {
		return ffloor(IntegerStruct.ONE);
	}

	public abstract QuotientRemainderResult ffloor(final RealStruct divisor);

	public QuotientRemainderResult ceiling() {
		return ceiling(IntegerStruct.ONE);
	}

	public abstract QuotientRemainderResult ceiling(final RealStruct divisor);

	public QuotientRemainderResult fceiling() {
		return fceiling(IntegerStruct.ONE);
	}

	public abstract QuotientRemainderResult fceiling(final RealStruct divisor);

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

	public abstract QuotientRemainderResult round(final RealStruct divisor);

	public QuotientRemainderResult fround() {
		return fround(IntegerStruct.ONE);
	}

	public abstract QuotientRemainderResult fround(final RealStruct divisor);

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
		final double doubleValue = doubleValue();
		final double exp = FastMath.exp(doubleValue);
		return new FloatStruct(exp);
	}

	@Override
	public RealStruct log() {
		final double doubleValue = doubleValue();
		final double log = FastMath.log(doubleValue);
		return new FloatStruct(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		if (base instanceof RealStruct) {
			final double number = doubleValue();
			final double baseVal = ((RealStruct) base).doubleValue();
			final double log = FastMath.log(baseVal, number);
			return new FloatStruct(log);
		}
		return super.log(base);
	}

	@Override
	public NumberStruct sqrt() {
		if (minusp()) {
			return new ComplexStruct(FloatStruct.ZERO, (RealStruct) negation().sqrt());
		}
		final double doubleValue = doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		return new FloatStruct(sqrt);
	}

	@Override
	public RealStruct sin() {
		final double doubleValue = doubleValue();
		final double sin = FastMath.sin(doubleValue);
		return new FloatStruct(sin);
	}

	@Override
	public RealStruct cos() {
		final double doubleValue = doubleValue();
		final double cos = FastMath.cos(doubleValue);
		return new FloatStruct(cos);
	}

	@Override
	public RealStruct tan() {
		final double doubleValue = doubleValue();
		final double tan = FastMath.tan(doubleValue);
		return new FloatStruct(tan);
	}

	@Override
	public RealStruct asin() {
		final double doubleValue = doubleValue();
		final double asin = FastMath.asin(doubleValue);
		return new FloatStruct(asin);
	}

	@Override
	public RealStruct acos() {
		final double doubleValue = doubleValue();
		final double acos = FastMath.acos(doubleValue);
		return new FloatStruct(acos);
	}

	@Override
	public RealStruct atan() {
		final double doubleValue = doubleValue();
		final double atan = FastMath.atan(doubleValue);
		return new FloatStruct(atan);
	}

	public RealStruct atan(final RealStruct real) {
		final double doubleValue = doubleValue();
		final double doubleValue2 = real.doubleValue();

		final double atan = FastMath.atan2(doubleValue, doubleValue2);
		return new FloatStruct(atan);
	}

	@Override
	public RealStruct sinh() {
		final double doubleValue = doubleValue();
		final double sinh = FastMath.sinh(doubleValue);
		return new FloatStruct(sinh);
	}

	@Override
	public RealStruct cosh() {
		final double doubleValue = doubleValue();
		final double cosh = FastMath.cosh(doubleValue);
		return new FloatStruct(cosh);
	}

	@Override
	public RealStruct tanh() {
		final double doubleValue = doubleValue();
		final double tanh = FastMath.tanh(doubleValue);
		return new FloatStruct(tanh);
	}

	@Override
	public RealStruct asinh() {
		final double doubleValue = doubleValue();
		final double asinh = FastMath.asinh(doubleValue);
		return new FloatStruct(asinh);
	}

	@Override
	public RealStruct acosh() {
		final double doubleValue = doubleValue();
		final double acosh = FastMath.acosh(doubleValue);
		return new FloatStruct(acosh);
	}

	@Override
	public RealStruct atanh() {
		final double doubleValue = doubleValue();
		final double atanh = FastMath.atanh(doubleValue);
		return new FloatStruct(atanh);
	}

	// Strategy Implementations

	protected abstract static class RealAddStrategy<S extends RealStruct> extends AddStrategy<S> {

		public RealStruct add(final S number1, final RealStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return add(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return add(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return add(number1, (RatioStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Add Operation.");
			}
		}

		@Override
		public abstract RealStruct add(final S number1, final IntegerStruct number2);

		@Override
		public RealStruct add(final S number1, final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return new FloatStruct(add);
		}

		@Override
		public abstract RealStruct add(final S number1, final RatioStruct number2);

		@Override
		public NumberStruct add(final S number1, final ComplexStruct number2) {
			final RealStruct real = number2.getReal();
			final RealStruct imaginary = number2.getImaginary();

			final NumberStruct add = number1.add(real);
			return new ComplexStruct((RealStruct) add, imaginary);
		}
	}

	protected abstract static class RealSubtractStrategy<S extends RealStruct> extends SubtractStrategy<S> {

		public RealStruct subtract(final S number1, final RealStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return subtract(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return subtract(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return subtract(number1, (RatioStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Subtract Operation.");
			}
		}

		@Override
		public abstract RealStruct subtract(final S number1, final IntegerStruct number2);

		@Override
		public RealStruct subtract(final S number1, final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}

		@Override
		public abstract RealStruct subtract(final S number1, final RatioStruct number2);

		@Override
		public NumberStruct subtract(final S number1, final ComplexStruct number2) {
			final RealStruct real = number2.getReal();
			final RealStruct imaginary = number2.getImaginary();

			final RealStruct zeroValue = number1.zeroValue();
			final NumberStruct subtractReal = number1.subtract(real);
			final NumberStruct subtractImag = zeroValue.subtract(imaginary);
			return new ComplexStruct((RealStruct) subtractReal, (RealStruct) subtractImag);
		}
	}

	protected abstract static class RealMultiplyStrategy<S extends RealStruct> extends MultiplyStrategy<S> {

		public RealStruct multiply(final S number1, final RealStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return multiply(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return multiply(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return multiply(number1, (RatioStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Multiply Operation.");
			}
		}

		@Override
		public abstract RealStruct multiply(final S number1, final IntegerStruct number2);

		@Override
		public RealStruct multiply(final S number1, final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return new FloatStruct(multiply);
		}

		@Override
		public abstract RealStruct multiply(final S number1, final RatioStruct number2);

		@Override
		public NumberStruct multiply(final S number1, final ComplexStruct number2) {
			final RealStruct real = number2.getReal();
			final RealStruct imaginary = number2.getImaginary();

			final NumberStruct multiplyReal = number1.multiply(real);
			final NumberStruct multiplyImag = number1.multiply(imaginary);
			return new ComplexStruct((RealStruct) multiplyReal, (RealStruct) multiplyImag);
		}
	}

	protected abstract static class RealDivideStrategy<S extends RealStruct> extends DivideStrategy<S> {

		public RealStruct divide(final S number1, final RealStruct number2) {
			if (number2 instanceof IntegerStruct) {
				return divide(number1, (IntegerStruct) number2);
			} else if (number2 instanceof FloatStruct) {
				return divide(number1, (FloatStruct) number2);
			} else if (number2 instanceof RatioStruct) {
				return divide(number1, (RatioStruct) number2);
			} else {
				throw new RuntimeException("Unsupported Number Type for Divide Operation.");
			}
		}

		@Override
		public abstract RealStruct divide(final S number1, final IntegerStruct number2);

		@Override
		public RealStruct divide(final S number1, final FloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}

		@Override
		public abstract RealStruct divide(final S number1, final RatioStruct number2);

		@Override
		public NumberStruct divide(final S number1, final ComplexStruct number2) {
			final RealStruct real = number2.getReal();
			final RealStruct imaginary = number2.getImaginary();
			final NumberStruct squareReal = real.multiply(real);
			final NumberStruct squareImag = imaginary.multiply(imaginary);

			final NumberStruct squareRealSquareImagSum = squareReal.add(squareImag);

			final NumberStruct multiplyReal = number1.multiply(real);
			final NumberStruct divideRealProductBySquareSum = multiplyReal.divide(squareRealSquareImagSum);

			final RealStruct zeroValue = number1.zeroValue();
			final NumberStruct subtractNum1FromZero = zeroValue.subtract(number1);
			final NumberStruct multiplyImag = subtractNum1FromZero.multiply(imaginary);
			final NumberStruct divideImagProductBySquareSum = multiplyImag.divide(squareRealSquareImagSum);

			// NOTE: The casting here should be safe as we are dealing with only 'real numbers' above anyways
			return new ComplexStruct((RealStruct) divideRealProductBySquareSum, (RealStruct) divideImagProductBySquareSum);
		}
	}

	protected abstract static class LessThanStrategy<S extends RealStruct> {

		public boolean lessThan(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return lessThan(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return lessThan(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return lessThan(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for LessThan Operation.");
			}
		}

		public abstract boolean lessThan(S real1, IntegerStruct real2);

		public abstract boolean lessThan(S real1, FloatStruct real2);

		public abstract boolean lessThan(S real1, RatioStruct real2);
	}

	protected abstract static class GreaterThanStrategy<S extends RealStruct> {

		public boolean greaterThan(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return greaterThan(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return greaterThan(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return greaterThan(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for GreaterThan Operation.");
			}
		}

		public abstract boolean greaterThan(S real1, IntegerStruct real2);

		public abstract boolean greaterThan(S real1, FloatStruct real2);

		public abstract boolean greaterThan(S real1, RatioStruct real2);
	}

	protected abstract static class LessThanOrEqualToStrategy<S extends RealStruct> {

		public boolean lessThanOrEqualTo(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return lessThanOrEqualTo(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return lessThanOrEqualTo(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return lessThanOrEqualTo(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for LessThanOrEqualTo Operation.");
			}
		}

		public abstract boolean lessThanOrEqualTo(S real1, IntegerStruct real2);

		public abstract boolean lessThanOrEqualTo(S real1, FloatStruct real2);

		public abstract boolean lessThanOrEqualTo(S real1, RatioStruct real2);
	}

	protected abstract static class GreaterThanOrEqualToStrategy<S extends RealStruct> {

		public boolean greaterThanOrEqualTo(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return greaterThanOrEqualTo(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return greaterThanOrEqualTo(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return greaterThanOrEqualTo(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for GreaterThanOrEqualTo Operation.");
			}
		}

		public abstract boolean greaterThanOrEqualTo(S real1, IntegerStruct real2);

		public abstract boolean greaterThanOrEqualTo(S real1, FloatStruct real2);

		public abstract boolean greaterThanOrEqualTo(S real1, RatioStruct real2);
	}

	protected static class MaxStrategy<S extends RealStruct> {

		protected static final MaxStrategy<RealStruct> INSTANCE = new MaxStrategy<>();

		public RealStruct max(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return max(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return max(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return max(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for Max Operation.");
			}
		}

		public RealStruct max(final S real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal max = bigDecimal1.max(bigDecimal2);
			return Objects.equals(bigDecimal1, max) ? real1 : real2;
		}

		public RealStruct max(final S real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal max = bigDecimal1.max(bigDecimal2);
			return Objects.equals(bigDecimal1, max) ? real1 : real2;
		}

		public RealStruct max(final S real1, final RatioStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal max = bigDecimal1.max(bigDecimal2);
			return Objects.equals(bigDecimal1, max) ? real1 : real2;
		}
	}

	protected static class MinStrategy<S extends RealStruct> {

		protected static final MinStrategy<RealStruct> INSTANCE = new MinStrategy<>();

		public RealStruct min(final S real1, final RealStruct real2) {
			if (real2 instanceof IntegerStruct) {
				return min(real1, (IntegerStruct) real2);
			} else if (real2 instanceof FloatStruct) {
				return min(real1, (FloatStruct) real2);
			} else if (real2 instanceof RatioStruct) {
				return min(real1, (RatioStruct) real2);
			} else {
				throw new RuntimeException("Unsupported Real Type for Min Operation.");
			}
		}

		public RealStruct min(final S real1, final IntegerStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal min = bigDecimal1.min(bigDecimal2);
			return Objects.equals(bigDecimal1, min) ? real1 : real2;
		}

		public RealStruct min(final S real1, final FloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal min = bigDecimal1.min(bigDecimal2);
			return Objects.equals(bigDecimal1, min) ? real1 : real2;
		}

		public RealStruct min(final S real1, final RatioStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();

			final BigDecimal min = bigDecimal1.min(bigDecimal2);
			return Objects.equals(bigDecimal1, min) ? real1 : real2;
		}
	}

	protected abstract static class QuotientRemainderStrategy<S extends RealStruct> {

		public QuotientRemainderResult floor(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.FLOOR, false);
		}

		public QuotientRemainderResult ffloor(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.FLOOR, true);
		}

		public QuotientRemainderResult ceiling(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.CEILING, false);
		}

		public QuotientRemainderResult fceiling(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.CEILING, true);
		}

		public QuotientRemainderResult round(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.HALF_EVEN, false);
		}

		public QuotientRemainderResult fround(final S real, final RealStruct divisor) {
			return quotientRemainder(real, divisor, RoundingMode.HALF_EVEN, true);
		}

		public QuotientRemainderResult quotientRemainder(final S real, final RealStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			if (divisor instanceof IntegerStruct) {
				return quotientRemainder(real, (IntegerStruct) divisor, roundingMode, isFloatResult);
			} else if (divisor instanceof FloatStruct) {
				return quotientRemainder(real, (FloatStruct) divisor, roundingMode, isFloatResult);
			} else if (divisor instanceof RatioStruct) {
				return quotientRemainder(real, (RatioStruct) divisor, roundingMode, isFloatResult);
			} else {
				throw new RuntimeException("Unsupported Real Type for Quotient/Remainder Operation.");
			}
		}

		public abstract QuotientRemainderResult quotientRemainder(final S real, final IntegerStruct divisor,
		                                                          final RoundingMode roundingMode,
		                                                          final boolean isFloatResult);

		public QuotientRemainderResult quotientRemainder(final S real, final FloatStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isFloatResult) {
			return floatQuotientRemainder(real, divisor, roundingMode, isFloatResult);
		}

		protected static QuotientRemainderResult floatQuotientRemainder(final RealStruct real, final RealStruct divisor,
		                                                                final RoundingMode roundingMode,
		                                                                final boolean isFloatResult) {
			final BigDecimal realBigDecimal = real.bigDecimalValue();
			final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();

			final BigDecimal quotient = realBigDecimal.divide(divisorBigDecimal, 0, roundingMode);
			final BigDecimal remainder = realBigDecimal.subtract(divisorBigDecimal.multiply(quotient));

			final RealStruct quotientReal;
			if (isFloatResult) {
				quotientReal = getFloatQuotient(real, divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = new IntegerStruct(quotientBigInteger);
			}

			final FloatStruct remainderFloat = new FloatStruct(remainder);
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		private static RealStruct getFloatQuotient(final RealStruct real, final RealStruct divisor,
		                                           final BigDecimal quotient) {
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


		public abstract QuotientRemainderResult quotientRemainder(final S real, final RatioStruct divisor,
		                                                          final RoundingMode roundingMode,
		                                                          final boolean isFloatResult);
	}

	protected static class RealExptStrategy<S extends RealStruct> extends ExptStrategy<S> {

		protected static final RealExptStrategy<RealStruct> INSTANCE = new RealExptStrategy<>();

		@Override
		public NumberStruct expt(final S number1, final IntegerStruct number2) {
			return exptInteger(number1, number2);
		}

		@Override
		public NumberStruct expt(final S number1, final FloatStruct number2) {
			return exptFloatRatio(number1, number2);
		}

		@Override
		public NumberStruct expt(final S number1, final RatioStruct number2) {
			return exptFloatRatio(number1, number2);
		}

		private static NumberStruct exptFloatRatio(final RealStruct real1, final RealStruct real2) {
			final double x = real1.doubleValue();
			final double y = real2.doubleValue();

			double result = FastMath.pow(x, y);
			if (Double.isNaN(result)) {
				if (x < 0) {
					result = FastMath.pow(-x, y);
					final double realPart = result * FastMath.cos(y * Math.PI);
					final double imagPart = result * FastMath.sin(y * Math.PI);

					final BigDecimal realBigDecimal = new BigDecimal(realPart);
					final FloatStruct real = new FloatStruct(realBigDecimal);
					final BigDecimal imagBigDecimal = new BigDecimal(imagPart);
					final FloatStruct imaginary = new FloatStruct(imagBigDecimal);
					return new ComplexStruct(real, imaginary);
				}
			}
			final BigDecimal resultBigDecimal = new BigDecimal(result);
			return new FloatStruct(resultBigDecimal);
		}

		@Override
		public NumberStruct expt(final S number1, final ComplexStruct number2) {
			final RealStruct powerComplexReal = number2.getReal();
			final BigDecimal powerComplexRealBigDecimal = powerComplexReal.bigDecimalValue();
			final FloatStruct real = new FloatStruct(powerComplexRealBigDecimal);

			final RealStruct powerComplexImaginary = number2.getImaginary();
			final BigDecimal powerComplexImaginaryBigDecimal = powerComplexImaginary.bigDecimalValue();
			final FloatStruct imaginary = new FloatStruct(powerComplexImaginaryBigDecimal);

			final ComplexStruct newPowerComplex = new ComplexStruct(real, imaginary);

			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final RealStruct base = new FloatStruct(bigDecimal1);
			final RealStruct logOfBase = base.log();
			final NumberStruct powerComplexLogOfBaseProduct = newPowerComplex.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}
	}

	// Static Multi-Arg Methods

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
}
