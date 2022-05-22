package jcl.lang;

import jcl.lang.internal.ComplexStructImpl;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public interface ComplexStruct extends NumberStruct {

	/**
	 * Returns a new ComplexStruct representation with the provided real and imaginary values.
	 *
	 * @param real
	 * 		the real part of the complex number
	 * @param imaginary
	 * 		the imaginary part of the complex number
	 *
	 * @return a new ComplexStruct representation with the provided real and imaginary values
	 */
	static ComplexStruct toLispComplex(final RealStruct real, final RealStruct imaginary) {
		return new ComplexStructImpl(real, imaginary);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof ComplexStruct)
						&& ((ComplexStruct) object).ap().equals(ap()));
	}
}
