package jcl.structs.numbers;

import jcl.types.numbers.Complex;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The {@code ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	private final Number real;
	private final Number imaginary;

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigInteger real, final BigInteger imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigInteger real, final BigDecimal imaginary) {
		super(Complex.INSTANCE, null, null);
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.multiply(BigInteger.TEN);
		this.real = new BigDecimal(realWithScale, 1);
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigInteger real, final BigFraction imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigDecimal real, final BigInteger imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;

		final BigInteger imaginaryWithScale = imaginary.multiply(BigInteger.TEN);
		this.imaginary = new BigDecimal(imaginaryWithScale, 1);
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigDecimal real, final BigDecimal imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigDecimal real, final BigFraction imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary.bigDecimalValue();
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigFraction real, final BigInteger imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigFraction real, final BigDecimal imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real.bigDecimalValue();
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	public ComplexStruct(final BigFraction real, final BigFraction imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * This method returns the {@code Number} value representative of the real part.
	 *
	 * @return the {@code Number} value representative of the real part
	 */
	public Number getReal() {
		return real;
	}

	/**
	 * This method returns the {@code Number} value representative of the imaginary part.
	 *
	 * @return the {@code Number} value representative of the imaginary part
	 */
	public Number getImaginary() {
		return imaginary;
	}

	@Override
	public String toString() {
		return "ComplexStruct{"
				+ "real=" + real
				+ ", imaginary=" + imaginary
				+ '}';
	}
}
