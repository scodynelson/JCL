package jcl.structs.numbers;

import jcl.structs.LispStruct;
import jcl.types.numbers.Rational;

import java.util.List;

/**
 * The {@code RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public class RationalStruct extends RealStruct {

	protected RationalStruct(final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(Rational.INSTANCE, directSuperClasses, subClasses);
	}

	protected RationalStruct(final Rational type,
							 final List<Class<LispStruct>> directSuperClasses, final List<Class<LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}
}
