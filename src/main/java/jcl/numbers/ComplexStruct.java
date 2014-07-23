package jcl.numbers;

import jcl.types.Complex;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	private final RealStruct real;
	private final RealStruct imaginary;

	/**
	 * Static 'getInstance' method.
	 *
	 * @param real      a {@link RealStruct} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link RealStruct} that represents the value of imaginary part {@code ComplexStruct}
	 * @return a new ComplexStruct
	 */
	public static ComplexStruct getInstance(final RealStruct real, final RealStruct imaginary) {
		if ((real instanceof IntegerStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((IntegerStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((IntegerStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof IntegerStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((IntegerStruct) real, (RatioStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((FloatStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((FloatStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof FloatStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((FloatStruct) real, (RatioStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof IntegerStruct)) {
			return new ComplexStruct((RatioStruct) real, (IntegerStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof FloatStruct)) {
			return new ComplexStruct((RatioStruct) real, (FloatStruct) imaginary);
		} else if ((real instanceof RatioStruct) && (imaginary instanceof RatioStruct)) {
			return new ComplexStruct((RatioStruct) real, (RatioStruct) imaginary);
		} else {
			throw new RuntimeException("Only reals are valid tokens for #c.");
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final IntegerStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final IntegerStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.getBigInteger().multiply(BigInteger.TEN);
		this.real = new FloatStruct(new BigDecimal(realWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final IntegerStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final FloatStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;

		final BigInteger imaginaryWithScale = imaginary.getBigInteger().multiply(BigInteger.TEN);
		this.imaginary = new FloatStruct(new BigDecimal(imaginaryWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final FloatStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final FloatStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = new FloatStruct(imaginary.getBigFraction().bigDecimalValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final RatioStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final RatioStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = new FloatStruct(real.getBigFraction().bigDecimalValue());
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final RatioStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * This method returns the {@link Number} value representative of the real part.
	 *
	 * @return the {@link Number} value representative of the real part
	 */
	public RealStruct getReal() {
		return real;
	}

	/**
	 * This method returns the {@link Number} value representative of the imaginary part.
	 *
	 * @return the {@link Number} value representative of the imaginary part
	 */
	public RealStruct getImaginary() {
		return imaginary;
	}

	@Override
	public String printStruct() {
		return "#C(" + real.printStruct() + ' ' + imaginary.printStruct() + ')';
	}

	@Override
	public String toString() {
		return "ComplexStruct{"
				+ "real=" + real
				+ ", imaginary=" + imaginary
				+ '}';
	}
}
