package jcl.lang;

import jcl.lang.internal.ConsStructImpl;

/**
 * The {@link ConsStruct} is the object representation of a Lisp 'cons' type.
 */
public interface ConsStruct extends ListStruct {

	ConsStruct rplaca(final LispStruct car);

	ConsStruct rplacd(final LispStruct cdr);

	static LispStruct acons(final LispStruct key, final LispStruct datum, final ListStruct alist) {
		final ConsStruct pair = toLispCons(key, datum);
		return toLispCons(pair, alist);
	}

	static ConsStruct toLispCons(final LispStruct lispStruct1, final LispStruct lispStruct2) {
		return new ConsStructImpl(lispStruct1, lispStruct2);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ConsStruct) {
			final ConsStruct objectCons = (ConsStruct) object;
			return car().equal(objectCons.car()) && cdr().equal(objectCons.cdr());
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ConsStruct) {
			final ConsStruct objectCons = (ConsStruct) object;
			return car().equalp(objectCons.car()) && cdr().equalp(objectCons.cdr());
		}
		return false;
	}
}
