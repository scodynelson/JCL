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
import jcl.types.FloatType;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

	public static final FloatStruct ZERO = new FloatStruct(BigDecimal.ZERO);

	public static final FloatStruct MINUS_ZERO = new FloatStruct(BigDecimal.valueOf(-0.0));

	public static final FloatStruct ONE = new FloatStruct(BigDecimal.ONE);

	/**
	 * The internal {@link BigDecimal} containing the float contents.
	 */
	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloatType.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatType
	 * 		a {@link FloatType} that represents the type of {@link FloatType}
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final FloatType floatType, final BigDecimal bigDecimal) {
		super(floatType, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
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
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		return new FloatStruct(bigDecimal.negate());
	}

	@Override
	public RationalStruct rational() {

		final BigFraction bigFraction = BigFractionUtil.getBigFraction(bigDecimal);
		if (bigFraction.getDenominator().equals(BigInteger.ONE)) {
			return new IntegerStruct(bigFraction.getNumerator());
		}
		return new RatioStruct(bigFraction);
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal max = bigDecimal.max(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal max = bigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal max = bigDecimal.max(((FloatStruct) real).bigDecimal);
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal min = bigDecimal.min(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal min = bigDecimal.min(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal min = bigDecimal.min(((FloatStruct) real).bigDecimal);
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public double doubleValue() {
		return bigDecimal.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public boolean plusp() {
		return bigDecimal.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigDecimal.signum() < 0;
	}

	@Override
	public boolean zerop() {
		return bigDecimal.signum() == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.add(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.add(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.add(((RatioStruct) number).getBigFraction().bigDecimalValue()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.subtract(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.subtract(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.subtract(((RatioStruct) number).getBigFraction().bigDecimalValue()));
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
			return new FloatStruct(bigDecimal.multiply(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.multiply(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.multiply(((RatioStruct) number).getBigFraction().bigDecimalValue()));
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
			return new FloatStruct(bigDecimal.divide(new BigDecimal(((IntegerStruct) number).getBigInteger()), MathContext.DECIMAL128));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.divide(((FloatStruct) number).bigDecimal, MathContext.DECIMAL128));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.divide(((RatioStruct) number).getBigFraction().bigDecimalValue(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			final NumberStruct re = c.getReal();
			final NumberStruct im = c.getImaginary();
			final NumberStruct denom = re.multiply(re).add(im.multiply(im));
			final NumberStruct resX = multiply(re).divide(denom);
			final NumberStruct resY = multiply(new IntegerStruct(BigInteger.valueOf(-1))).multiply(im).divide(denom);
			return new ComplexStruct((RealStruct) resX, (RealStruct) resY);
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) == 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) == 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isEqualTo(obj);
		}
		if (obj instanceof ComplexStruct) {
			return ((ComplexStruct) obj).isEqualTo(this);
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
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) < 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) < 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isLessThan(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) > 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) > 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isGreaterThan(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) <= 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isLessThanOrEqualTo(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) >= 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isGreaterThanOrEqualTo(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct exp() {
		final double doubleValue = bigDecimal.doubleValue();
		final double exp = FastMath.exp(doubleValue);
		return new FloatStruct(new BigDecimal(exp));
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			return ONE;
		}
		if (zerop()) {
			return this;
		}
		if (isEqualTo(ONE)) {
			return this;
		}

		NumberStruct newPower = power;
		if (newPower instanceof ComplexStruct) {
			final ComplexStruct powerComplex = (ComplexStruct) newPower;
			newPower = new ComplexStruct(
					new FloatStruct(BigDecimal.valueOf(powerComplex.getReal().doubleValue())),
					new FloatStruct(BigDecimal.valueOf(powerComplex.getImaginary().doubleValue())));
		} else {
			final RealStruct powerReal = (RealStruct) newPower;
			newPower = new FloatStruct(BigDecimal.valueOf(powerReal.doubleValue()));
		}

		final NumberStruct base = new FloatStruct(BigDecimal.valueOf(doubleValue()));

		if (newPower instanceof ComplexStruct) {
			return newPower.multiply(base.log()).exp();
		}

		final double x = ((FloatStruct) base).doubleValue(); // base
		final double y = ((FloatStruct) newPower).doubleValue(); // power

		double r = FastMath.pow(x, y);
		if (Double.isNaN(r)) {
			if (x < 0) {
				r = FastMath.pow(-x, y);
				final double realPart = r * FastMath.cos(y * Math.PI);
				final double imagPart = r * FastMath.sin(y * Math.PI);
				return new ComplexStruct
						(new FloatStruct(BigDecimal.valueOf(realPart)),
								new FloatStruct(BigDecimal.valueOf(imagPart)));
			}
		}
		return new FloatStruct(BigDecimal.valueOf(r));
	}

	@Override
	public NumberStruct sqrt() {
		final double doubleValue = bigDecimal.doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		return new FloatStruct(new BigDecimal(sqrt));
	}

	@Override
	public NumberStruct log() {
		final double doubleValue = bigDecimal.doubleValue();
		final double log = FastMath.log(doubleValue);
		return new FloatStruct(new BigDecimal(log));
	}

	@Override
	public NumberStruct sin() {
		final double doubleValue = bigDecimal.doubleValue();
		final double sin = FastMath.sin(doubleValue);
		return new FloatStruct(new BigDecimal(sin));
	}

	@Override
	public NumberStruct cos() {
		final double doubleValue = bigDecimal.doubleValue();
		final double cos = FastMath.cos(doubleValue);
		return new FloatStruct(new BigDecimal(cos));
	}

	@Override
	public NumberStruct tan() {
		final double doubleValue = bigDecimal.doubleValue();
		final double tan = FastMath.tan(doubleValue);
		return new FloatStruct(new BigDecimal(tan));
	}

	@Override
	public NumberStruct asin() {
		final double doubleValue = bigDecimal.doubleValue();
		final double asin = FastMath.asin(doubleValue);
		return new FloatStruct(new BigDecimal(asin));
	}

	@Override
	public NumberStruct acos() {
		final double doubleValue = bigDecimal.doubleValue();
		final double acos = FastMath.acos(doubleValue);
		return new FloatStruct(new BigDecimal(acos));
	}

	@Override
	public NumberStruct atan() {
		final double doubleValue = bigDecimal.doubleValue();
		final double atan = FastMath.atan(doubleValue);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final double doubleValue = bigDecimal.doubleValue();
		final double doubleValue2 = real.doubleValue();

		final double atan = FastMath.atan2(doubleValue, doubleValue2);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public NumberStruct sinh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double sinh = FastMath.sinh(doubleValue);
		return new FloatStruct(new BigDecimal(sinh));
	}

	@Override
	public NumberStruct cosh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double cosh = FastMath.cosh(doubleValue);
		return new FloatStruct(new BigDecimal(cosh));
	}

	@Override
	public NumberStruct tanh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double tanh = FastMath.tanh(doubleValue);
		return new FloatStruct(new BigDecimal(tanh));
	}

	@Override
	public NumberStruct asinh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double asinh = FastMath.asinh(doubleValue);
		return new FloatStruct(new BigDecimal(asinh));
	}

	@Override
	public NumberStruct acosh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double acosh = FastMath.acosh(doubleValue);
		return new FloatStruct(new BigDecimal(acosh));
	}

	@Override
	public NumberStruct atanh() {
		final double doubleValue = bigDecimal.doubleValue();
		final double atanh = FastMath.atanh(doubleValue);
		return new FloatStruct(new BigDecimal(atanh));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigDecimal)
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
		final FloatStruct rhs = (FloatStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigDecimal, rhs.bigDecimal)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigDecimal)
		                                                                .toString();
	}
}
