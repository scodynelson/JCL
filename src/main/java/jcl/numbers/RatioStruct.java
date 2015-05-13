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
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

	public static final RatioStruct ZERO = new RatioStruct(BigFraction.ZERO);

	public static final RatioStruct ONE = new RatioStruct(BigFraction.ONE);

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	private final BigFraction bigFraction;

	/**
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the numerator value of the RatioStruct
	 * @param denominator
	 * 		the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		this(new BigFraction(numerator, denominator));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(RatioType.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
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
		final BigFraction abs = bigFraction.abs();
		if (abs.equals(bigFraction)) {
			return this;
		}
		return new RatioStruct(abs);
	}

	@Override
	public double doubleValue() {
		return bigFraction.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigFraction.bigDecimalValue();
	}

	@Override
	public boolean plusp() {
		final BigFraction abs = bigFraction.abs();
		return abs.equals(bigFraction);
	}

	@Override
	public boolean minusp() {
		final BigFraction abs = bigFraction.abs();
		return !abs.equals(bigFraction);
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(((RatioStruct) real).bigFraction.bigDecimalValue());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(((RatioStruct) real).bigFraction.bigDecimalValue());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public IntegerStruct numerator() {
		return new IntegerStruct(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(bigFraction.getDenominator());
	}

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.equals(bigFraction);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigFraction n = bigFraction.add(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.add(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().add(((FloatStruct) number).getBigDecimal()));
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
			final BigFraction n = bigFraction.subtract(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.subtract(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().subtract(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) subtract(c.getReal()),
					(RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigFraction n = bigFraction.multiply(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.multiply(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().multiply(((FloatStruct) number).getBigDecimal()));
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
			final BigFraction n = bigFraction.divide(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.divide(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			if (number.zerop()) {
				throw new RuntimeException("division by zero");
			}
			return new FloatStruct(bigFraction.bigDecimalValue().divide(((FloatStruct) number).getBigDecimal(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			// numerator
			final NumberStruct realPart = multiply(c.getReal());
			final NumberStruct imagPart = new IntegerStruct(BigInteger.ZERO).subtract(this).multiply(c.getImaginary());
			// denominator
			NumberStruct d = c.getReal().multiply(c.getReal());
			d = d.add(c.getImaginary().multiply(c.getImaginary()));
			return new ComplexStruct((RealStruct) realPart.divide(d), (RealStruct) imagPart.divide(d));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof RatioStruct) {
			return bigFraction.equals(((RatioStruct) obj).bigFraction);
		}
		if (obj instanceof FloatStruct) {
			return isEqualTo(((FloatStruct) obj).rational());
		}
		if (obj instanceof IntegerStruct) {
			return bigFraction.equals(new BigFraction(((IntegerStruct) obj).getBigInteger()));
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
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) < 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) < 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) > 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) > 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) <= 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) >= 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct exp() {
		final double doubleValue = bigFraction.doubleValue();
		final double exp = FastMath.exp(doubleValue);
		return new FloatStruct(new BigDecimal(exp));
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}
		if (zerop()) {
			return this;
		}
		if (isEqualTo(ONE)) {
			return this;
		}

		if (power instanceof IntegerStruct) {
			// exact math version
			return intexp(this, (IntegerStruct) power);
		}

		// for anything not a rational or complex rational, use
		// float approximation.
		boolean wantDoubleFloat = false;
		if ((power instanceof FloatStruct) || ((power instanceof ComplexStruct)
				&& ((((ComplexStruct) power).getReal() instanceof FloatStruct)
				|| (((ComplexStruct) power).getImaginary() instanceof FloatStruct)))) {
			wantDoubleFloat = true;
		}

		final NumberStruct base;
		NumberStruct newPower = power;
		if (wantDoubleFloat) {
			if (newPower instanceof ComplexStruct) {
				final ComplexStruct powerComplex = (ComplexStruct) newPower;
				newPower = new ComplexStruct(
						new FloatStruct(BigDecimal.valueOf(powerComplex.getReal().doubleValue())),
						new FloatStruct(BigDecimal.valueOf(powerComplex.getImaginary().doubleValue())));
			} else {
				final RealStruct powerReal = (RealStruct) newPower;
				newPower = new FloatStruct(BigDecimal.valueOf(powerReal.doubleValue()));
			}

			base = new FloatStruct(BigDecimal.valueOf(doubleValue()));
		} else {
			base = this;
		}

		if (newPower instanceof ComplexStruct) {
			return newPower.multiply(base.log()).exp();
		}
		final double x; // base
		if (base instanceof RatioStruct) {
			x = ((RatioStruct) base).doubleValue();
		} else {
			x = ((FloatStruct) base).doubleValue();
		}

		final double y; // power
		if (newPower instanceof RatioStruct) {
			y = ((RatioStruct) newPower).doubleValue();
		} else if (newPower instanceof FloatStruct) {
			y = ((FloatStruct) newPower).doubleValue();
		} else {
			throw new RuntimeException("EXPT: unsupported case: power is of type " + newPower.getType());
		}

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
		final double doubleValue = bigFraction.doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		return new FloatStruct(new BigDecimal(sqrt));
	}

	@Override
	public NumberStruct log() {
		final double doubleValue = bigFraction.doubleValue();
		final double log = FastMath.log(doubleValue);
		return new FloatStruct(new BigDecimal(log));
	}

	@Override
	public NumberStruct sin() {
		final double doubleValue = bigFraction.doubleValue();
		final double sin = FastMath.sin(doubleValue);
		return new FloatStruct(new BigDecimal(sin));
	}

	@Override
	public NumberStruct cos() {
		final double doubleValue = bigFraction.doubleValue();
		final double cos = FastMath.cos(doubleValue);
		return new FloatStruct(new BigDecimal(cos));
	}

	@Override
	public NumberStruct tan() {
		final double doubleValue = bigFraction.doubleValue();
		final double tan = FastMath.tan(doubleValue);
		return new FloatStruct(new BigDecimal(tan));
	}

	@Override
	public NumberStruct asin() {
		final double doubleValue = bigFraction.doubleValue();
		final double asin = FastMath.asin(doubleValue);
		return new FloatStruct(new BigDecimal(asin));
	}

	@Override
	public NumberStruct acos() {
		final double doubleValue = bigFraction.doubleValue();
		final double acos = FastMath.acos(doubleValue);
		return new FloatStruct(new BigDecimal(acos));
	}

	@Override
	public NumberStruct atan() {
		final double doubleValue = bigFraction.doubleValue();
		final double atan = FastMath.atan(doubleValue);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final double doubleValue = bigFraction.doubleValue();
		final double doubleValue2 = real.doubleValue();

		final double atan = FastMath.atan2(doubleValue, doubleValue2);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public NumberStruct sinh() {
		final double doubleValue = bigFraction.doubleValue();
		final double sinh = FastMath.sinh(doubleValue);
		return new FloatStruct(new BigDecimal(sinh));
	}

	@Override
	public NumberStruct cosh() {
		final double doubleValue = bigFraction.doubleValue();
		final double cosh = FastMath.cosh(doubleValue);
		return new FloatStruct(new BigDecimal(cosh));
	}

	@Override
	public NumberStruct tanh() {
		final double doubleValue = bigFraction.doubleValue();
		final double tanh = FastMath.tanh(doubleValue);
		return new FloatStruct(new BigDecimal(tanh));
	}

	@Override
	public NumberStruct asinh() {
		final double doubleValue = bigFraction.doubleValue();
		final double asinh = FastMath.asinh(doubleValue);
		return new FloatStruct(new BigDecimal(asinh));
	}

	@Override
	public NumberStruct acosh() {
		final double doubleValue = bigFraction.doubleValue();
		final double acosh = FastMath.acosh(doubleValue);
		return new FloatStruct(new BigDecimal(acosh));
	}

	@Override
	public NumberStruct atanh() {
		final double doubleValue = bigFraction.doubleValue();
		final double atanh = FastMath.atanh(doubleValue);
		return new FloatStruct(new BigDecimal(atanh));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigFraction)
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
		final RatioStruct rhs = (RatioStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigFraction)
		                                                                .toString();
	}
}
