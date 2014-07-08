package jcl.numbers;

import jcl.types.Float;
import jcl.types.SingleFloat;

import java.math.BigDecimal;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param floatString the string value of the {@code FloatStruct}
	 */
	public FloatStruct(final String floatString) {
		this(SingleFloat.INSTANCE, new BigDecimal(floatString));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal the value of the {@code FloatStruct}
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloat.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatFormat a {@link Float} that represents the type of {@link Float}
	 * @param bigDecimal  the value of the {@code FloatStruct}
	 */
	public FloatStruct(final Float floatFormat, final BigDecimal bigDecimal) {
		super(floatFormat, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * This method returns the value of the {@code FloatStruct}.
	 *
	 * @return value of the {@code FloatStruct}
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}

	@Override
	public String toString() {
		return "FloatStruct{"
				+ ", bigDecimal=" + bigDecimal
				+ '}';
	}
}
