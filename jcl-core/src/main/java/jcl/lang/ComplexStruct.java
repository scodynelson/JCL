package jcl.lang;

import java.util.Arrays;

import jcl.lang.internal.number.ComplexStructImpl;
import org.apfloat.Apcomplex;
import org.apfloat.Apint;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public interface ComplexStruct extends NumberStruct {

	/**
	 * {@link ComplexStruct} constant representing I.
	 */
	ComplexStruct I = ComplexStructImpl.valueOf(Apcomplex.I, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing -I.
	 */
	ComplexStruct NEGATE_I = ComplexStructImpl.valueOf(Apcomplex.ZERO, new Apint(-1L), ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 0.
	 */
	ComplexStruct ZERO = ComplexStructImpl.valueOf(Apcomplex.ZERO, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 0.0.
	 */
	ComplexStruct ZERO_FLOAT = ComplexStructImpl.valueOf(Apcomplex.ZERO, ValueType.FLOAT);

	/**
	 * {@link ComplexStruct} constant representing 1.
	 */
	ComplexStruct ONE = ComplexStructImpl.valueOf(Apcomplex.ONE, ValueType.RATIONAL);

	/**
	 * {@link ComplexStruct} constant representing 1.0.
	 */
	ComplexStruct ONE_FLOAT = ComplexStructImpl.valueOf(Apcomplex.ONE, ValueType.FLOAT);

	enum ValueType {
		RATIONAL,
		FLOAT
	}

	ValueType getValueType();

	static ValueType determineComplexValueType(final RealStruct... reals) {
		final boolean anyFloats
				= Arrays.stream(reals)
				        .map(Object::getClass)
				        .anyMatch(FloatStruct.class::isAssignableFrom);

		return anyFloats ? ValueType.FLOAT : ValueType.RATIONAL;
	}

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof ComplexStruct)
						&& ((ComplexStruct) object).ap().equals(ap()));
	}
}
