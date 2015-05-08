/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;
import java.math.MathContext;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

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

	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

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
	public boolean zerop() {
		return BigFraction.ZERO.equals(bigFraction);
	}

	@Override
	public NumberStruct add(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			final BigFraction n = bigFraction.add(((IntegerStruct) numberStruct).getBigInteger());
			return new RatioStruct(n);
		}
		if (numberStruct instanceof RatioStruct) {
			final BigFraction n = bigFraction.add(((RatioStruct) numberStruct).bigFraction);
			return new RatioStruct(n);
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().add(((FloatStruct) numberStruct).getBigDecimal()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct subtract(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			final BigFraction n = bigFraction.subtract(((IntegerStruct) numberStruct).getBigInteger());
			return new RatioStruct(n);
		}
		if (numberStruct instanceof RatioStruct) {
			final BigFraction n = bigFraction.subtract(((RatioStruct) numberStruct).bigFraction);
			return new RatioStruct(n);
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().subtract(((FloatStruct) numberStruct).getBigDecimal()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) subtract(c.getReal()),
					(RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			final BigFraction n = bigFraction.multiply(((IntegerStruct) numberStruct).getBigInteger());
			return new RatioStruct(n);
		}
		if (numberStruct instanceof RatioStruct) {
			final BigFraction n = bigFraction.multiply(((RatioStruct) numberStruct).bigFraction);
			return new RatioStruct(n);
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().multiply(((FloatStruct) numberStruct).getBigDecimal()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) multiply(c.getReal()), (RealStruct) multiply(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct divide(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			final BigFraction n = bigFraction.divide(((IntegerStruct) numberStruct).getBigInteger());
			return new RatioStruct(n);
		}
		if (numberStruct instanceof RatioStruct) {
			final BigFraction n = bigFraction.divide(((RatioStruct) numberStruct).bigFraction);
			return new RatioStruct(n);
		}
		if (numberStruct instanceof FloatStruct) {
			if (numberStruct.zerop()) {
				throw new RuntimeException("division by zero");
			}
			return new FloatStruct(bigFraction.bigDecimalValue().divide(((FloatStruct) numberStruct).getBigDecimal(), MathContext.DECIMAL128));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
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
	public RealStruct truncate(final RealStruct obj) {
		// "When rationals and floats are combined by a numerical function,
		// the rational is first converted to a float of the same format."
		// 12.1.4.1
		if (obj instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue()).truncate(obj);
		}

		try {
			final BigInteger n;
			final BigInteger d;
			if (obj instanceof IntegerStruct) {
				n = ((IntegerStruct) obj).getBigInteger();
				d = BigInteger.ONE;
			} else if (obj instanceof RatioStruct) {
				n = ((RatioStruct) obj).bigFraction.getNumerator();
				d = ((RatioStruct) obj).bigFraction.getDenominator();
			} else {
				throw new TypeErrorException("Not of type NUMBER");
			}
			// Invert and multiply.
			final BigInteger num = bigFraction.getNumerator().multiply(d);
			final BigInteger den = bigFraction.getDenominator().multiply(n);
			final BigInteger quotient = num.divide(den);
			// Multiply quotient by divisor.
			final RationalStruct product = IntegerStruct.number(quotient.multiply(n), d);
			// Subtract to get remainder.
			final LispStruct remainder = subtract(product);

			return new IntegerStruct(quotient);
//			return remainder;
		} catch (final ArithmeticException e) {
			if (obj.zerop()) {
				throw new RuntimeException("division by zero");
			}
			throw new RuntimeException("arithmetic error", e);
		}
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
