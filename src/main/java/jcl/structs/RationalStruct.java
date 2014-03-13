package jcl.structs;

import jcl.types.Rational;

import java.util.List;

/**
 * The {@code RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public class RationalStruct extends RealStruct {

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected RationalStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Rational.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type               the type of the rational object
	 * @param directSuperClasses the direct super classes
	 * @param subClasses         the subclasses
	 */
	protected RationalStruct(final Rational type,
							 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public String toString() {
		return "RationalStruct{}";
	}
}
