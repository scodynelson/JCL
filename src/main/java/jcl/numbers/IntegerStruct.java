/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.Objects;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.IntegerType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4665072618932472349L;

	public static final IntegerStruct ZERO = new IntegerStruct(BigInteger.ZERO);

	public static final IntegerStruct ONE = new IntegerStruct(BigInteger.ONE);

	public static final IntegerStruct TWO = new IntegerStruct(BigInteger.valueOf(2));

	public static final IntegerStruct MINUS_ONE = new IntegerStruct(BigInteger.valueOf(-1));

	/**
	 * The internal {@link BigInteger} containing the float contents.
	 */
	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(IntegerType.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerType
	 * 		a {@link IntegerType} that represents the type of {@link IntegerType}
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final IntegerType integerType, final BigInteger bigInteger) {
		super(integerType, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equalp(final LispStruct lispStruct) {
		return isEqualTo(lispStruct);
	}

	@Override
	public RealStruct ABS() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		return new IntegerStruct(bigInteger.negate());
	}

	@Override
	public IntegerStruct numerator() {
		return this;
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(BigInteger.ONE);
	}

	public boolean evenp() {
		return !bigInteger.testBit(0);
	}

	public boolean oddp() {
		return bigInteger.testBit(0);
	}

	@Override
	public double doubleValue() {
		return bigInteger.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return new BigDecimal(bigInteger);
	}

	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigInteger.signum() < 0;
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal max = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal max = asBigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigInteger max = bigInteger.max(((IntegerStruct) real).bigInteger);
			if (Objects.equals(bigInteger, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal min = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal min = asBigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigInteger min = bigInteger.max(((IntegerStruct) real).bigInteger);
			if (Objects.equals(bigInteger, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.add(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigFraction bigFraction = ((RatioStruct) number).getBigFraction();
			final BigInteger numerator = bigFraction.getNumerator();
			final BigInteger denominator = bigFraction.getDenominator();
			return number(bigInteger.multiply(denominator).add(numerator), denominator);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).add(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}

		throw new TypeErrorException("Not of type NUMBER");
	}

	public static RationalStruct number(final BigInteger numerator, final BigInteger denominator) {

		if (denominator.signum() == 0) {
			throw new RuntimeException("division by zero");
		}
		BigInteger realNumerator = numerator;
		BigInteger realDenominator = denominator;
		if (realDenominator.signum() < 0) {
			realNumerator = realNumerator.negate();
			realDenominator = realDenominator.negate();
		}
		final BigInteger gcd = realNumerator.gcd(realDenominator);
		if (!gcd.equals(BigInteger.ONE)) {
			realNumerator = realNumerator.divide(gcd);
			realDenominator = realDenominator.divide(gcd);
		}
		if (realDenominator.equals(BigInteger.ONE)) {
			return new IntegerStruct(realNumerator);
		} else {
			return new RatioStruct(realNumerator, realDenominator);
		}
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.subtract(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigInteger numerator = ((RatioStruct) number).getBigFraction().getNumerator();
			final BigInteger denominator = ((RatioStruct) number).getBigFraction().getDenominator();
			return number(bigInteger.multiply(denominator).subtract(numerator),
					denominator);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).subtract(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) subtract(c.getReal()), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.multiply(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigInteger n = ((RatioStruct) number).getBigFraction().getNumerator();
			return number(n.multiply(bigInteger), ((RatioStruct) number).getBigFraction().getDenominator());
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).multiply(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) multiply(c.getReal()), (RealStruct) multiply(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return number(bigInteger, ((IntegerStruct) number).bigInteger);
		}
		if (number instanceof RatioStruct) {
			final BigInteger d = ((RatioStruct) number).getBigFraction().getDenominator();
			return number(d.multiply(bigInteger), ((RatioStruct) number).getBigFraction().getNumerator());
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).divide(((FloatStruct) number).getBigDecimal(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			final NumberStruct realPart = c.getReal();
			final NumberStruct imagPart = c.getImaginary();
			final NumberStruct denominator = realPart.multiply(realPart).add(imagPart.multiply(imagPart));
			return new ComplexStruct((RealStruct) multiply(realPart).divide(denominator), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(multiply(imagPart).divide(denominator)));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.equals(((IntegerStruct) obj).bigInteger);
		}
		if (obj instanceof FloatStruct) {
			return isEqualTo(((FloatStruct) obj).rational());
		}
		if (obj instanceof RatioStruct) {
			final RatioStruct ratioStruct = (RatioStruct) obj;
			final BigFraction bigFraction = ratioStruct.getBigFraction().reduce();
			return bigFraction.getDenominator().equals(BigInteger.ONE) && bigInteger.equals(bigFraction.getNumerator());
		}
		if (obj instanceof NumberStruct) {
			return false;
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isNotEqualTo(final LispStruct obj) {
		return !isEqualTo(obj);
	}

	@Override
	public boolean isLessThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) < 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) < 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) > 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) > 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) <= 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) >= 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return ONE;
			}
			return FloatStruct.ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		if (power instanceof IntegerStruct) {
			// exact math version
			return new IntegerStruct(ArithmeticUtils.pow(bigInteger, ((IntegerStruct) power).bigInteger));
		} else if (power instanceof ComplexStruct) {
			final ComplexStruct powerComplex = (ComplexStruct) power;

			final FloatStruct real = new FloatStruct(powerComplex.getReal().bigDecimalValue());
			final FloatStruct imaginary = new FloatStruct(powerComplex.getImaginary().bigDecimalValue());
			final ComplexStruct newPowerComplex = new ComplexStruct(real, imaginary);

			final RealStruct base = new FloatStruct(bigDecimalValue());
			return newPowerComplex.multiply(base.log()).exp();
		} else {
			final double x = doubleValue();
			final double y = ((RealStruct) power).doubleValue();

			double r = FastMath.pow(x, y);
			if (Double.isNaN(r)) {
				if (x < 0) {
					r = FastMath.pow(-x, y);
					final double realPart = r * FastMath.cos(y * Math.PI);
					final double imagPart = r * FastMath.sin(y * Math.PI);

					final FloatStruct real = new FloatStruct(BigDecimal.valueOf(realPart));
					final FloatStruct imaginary = new FloatStruct(BigDecimal.valueOf(imagPart));
					return new ComplexStruct(real, imaginary);
				}
			}
			return new FloatStruct(BigDecimal.valueOf(r));
		}
	}

	public IntegerStruct ash(final IntegerStruct obj) {
		final BigInteger count = obj.bigInteger;
		if (count.signum() > 0) {
			throw new RuntimeException("Can't represent result of left shift.");
		}
		if (count.signum() < 0) {
			return (bigInteger.signum() >= 0) ? ZERO : MINUS_ONE;
		} else {
			return this;
		}
	}

	public IntegerStruct LOGNOT() {
		return new IntegerStruct(bigInteger.not());
	}

	public IntegerStruct LOGAND(final IntegerStruct obj) {
		return new IntegerStruct(bigInteger.and(obj.bigInteger));
	}

	public IntegerStruct LOGIOR(final IntegerStruct obj) {
		return new IntegerStruct(bigInteger.or(obj.bigInteger));
	}

	public IntegerStruct LOGXOR(final IntegerStruct obj) {
		return new IntegerStruct(bigInteger.xor(obj.bigInteger));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigInteger)
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
		final IntegerStruct rhs = (IntegerStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigInteger, rhs.bigInteger)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigInteger)
		                                                                .toString();
	}
}
