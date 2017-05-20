package jcl.lang;

import jcl.lang.internal.ComplexStructImpl;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public interface ComplexStruct extends NumberStruct {

	static ComplexStruct toLispComplex(final RealStruct real, final RealStruct imaginary) {
		return new ComplexStructImpl(real, imaginary);
	}

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof ComplexStruct)
						&& ((ComplexStruct) object).ap().equals(ap()));
	}
}
