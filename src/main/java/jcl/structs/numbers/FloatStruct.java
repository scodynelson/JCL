package jcl.structs.numbers;

import jcl.types.Float;
import jcl.types.SingleFloat;

import java.math.BigDecimal;

/**
 * The {@code FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	private final BigDecimal bigDecimal;

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
	 * @param floatFormat a {@code Float} that represents the type of {@code Float}
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
