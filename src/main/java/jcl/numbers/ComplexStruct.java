/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.ComplexType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7848008215064899579L;

	/**
	 * The {@link RealStruct} that comprises the real value of the complex.
	 */
	private final RealStruct real;

	/**
	 * The {@link RealStruct} that comprises the imaginary value of the complex.
	 */
	private final RealStruct imaginary;

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
		this.real = real;
		this.imaginary = imaginary;
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
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.getBigInteger().multiply(BigInteger.TEN);
		this.real = new FloatStruct(new BigDecimal(realWithScale, 1));
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

		final BigInteger imaginaryWithScale = imaginary.getBigInteger().multiply(BigInteger.TEN);
		this.imaginary = new FloatStruct(new BigDecimal(imaginaryWithScale, 1));
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
		this.imaginary = new FloatStruct(imaginary.getBigFraction().bigDecimalValue());
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
		this.real = new FloatStruct(real.getBigFraction().bigDecimalValue());
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
		if (real.zerop()) {
			return imaginary.ABS();
		}

		final BigDecimal bdReal;
		if (real instanceof IntegerStruct) {
			bdReal = new BigDecimal(((IntegerStruct) real).getBigInteger());
		} else if (real instanceof FloatStruct) {
			bdReal = ((FloatStruct) real).getBigDecimal();
		} else if (real instanceof RatioStruct) {
			bdReal = ((RatioStruct) real).getBigFraction().bigDecimalValue();
		} else {
			throw new TypeErrorException("Not of type REAL");
		}

		final BigDecimal bdImag;
		if (imaginary instanceof IntegerStruct) {
			bdImag = new BigDecimal(((IntegerStruct) imaginary).getBigInteger());
		} else if (imaginary instanceof FloatStruct) {
			bdImag = ((FloatStruct) imaginary).getBigDecimal();
		} else if (imaginary instanceof RatioStruct) {
			bdImag = ((RatioStruct) imaginary).getBigFraction().bigDecimalValue();
		} else {
			throw new TypeErrorException("Not of type REAL");
		}

		// TODO: can we avoid possible precision loss here???
		final double dblReal = bdReal.doubleValue();
		final double dblImag = bdImag.doubleValue();
		return new FloatStruct(BigDecimal.valueOf(StrictMath.hypot(dblReal, dblImag)));
	}

	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	@Override
	public NumberStruct add(final NumberStruct numberStruct) {
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) real.add(c.real), (RealStruct) imaginary.add(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.add(numberStruct), imaginary);
	}

	@Override
	public NumberStruct subtract(final NumberStruct numberStruct) {
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) real.subtract(c.real), (RealStruct) imaginary.subtract(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.subtract(numberStruct), imaginary);
	}

	@Override
	public NumberStruct multiply(final NumberStruct numberStruct) {
		if (numberStruct instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) numberStruct).real;
			final NumberStruct d = ((ComplexStruct) numberStruct).imaginary;
			// xy = (ac - bd) + i(ad + bc)
			// real part = ac - bd
			// imag part = ad + bc
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct bc = b.multiply(c);
			return new ComplexStruct((RealStruct) ac.subtract(bd), (RealStruct) ad.add(bc));
		}
		return new ComplexStruct((RealStruct) real.multiply(numberStruct), (RealStruct) imaginary.multiply(numberStruct));
	}

	@Override
	public NumberStruct divide(final NumberStruct numberStruct) {
		if (numberStruct instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) numberStruct).real;
			final NumberStruct d = ((ComplexStruct) numberStruct).imaginary;
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct bc = b.multiply(c);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct denominator = c.multiply(c).add(d.multiply(d));
			return new ComplexStruct((RealStruct) ac.add(bd).divide(denominator), (RealStruct) bc.subtract(ad).divide(denominator));
		}
		return new ComplexStruct((RealStruct) real.divide(numberStruct), (RealStruct) imaginary.divide(numberStruct));
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) obj;
			return real.isEqualTo(c.real) && imaginary.isEqualTo(c.imaginary);
		}
		if (obj instanceof NumberStruct) {
			// obj is a number, but not complex.
			if (imaginary instanceof FloatStruct) {
				if (((FloatStruct) imaginary).getBigDecimal().compareTo(BigDecimal.ZERO) == 0) {
					if (obj instanceof IntegerStruct) {
						return new BigDecimal(((IntegerStruct) obj).getBigInteger()).compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
					if (obj instanceof FloatStruct) {
						return ((FloatStruct) obj).getBigDecimal().compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
				}
			}
			return false;
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isNotEqualTo(final LispStruct obj) {
		return !isEqualTo(obj);
	}

	@Override
	public NumberStruct sin() {
		return null;
	}

	@Override
	public NumberStruct cos() {
		return null;
	}

	@Override
	public NumberStruct tan() {
		return null;
	}

	@Override
	public NumberStruct asin() {
		return null;
	}

	@Override
	public NumberStruct acos() {
		return null;
	}

	@Override
	public NumberStruct atan() {
		return null;
	}

	@Override
	public NumberStruct sinh() {
		return null;
	}

	@Override
	public NumberStruct cosh() {
		return null;
	}

	@Override
	public NumberStruct tanh() {
		return null;
	}

	@Override
	public NumberStruct asinh() {
		return null;
	}

	@Override
	public NumberStruct acosh() {
		return null;
	}

	@Override
	public NumberStruct atanh() {
		return null;
	}

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

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(real)
		                                                                .append(imaginary)
		                                                                .toString();
	}
}
