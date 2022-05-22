package jcl.lang;

import jcl.lang.internal.ConsStructImpl;

/**
 * The {@link ConsStruct} is the object representation of a Lisp 'cons' type.
 */
public interface ConsStruct extends ListStruct {

	/**
	 * Replaces the car of the cons with the provided object.
	 *
	 * @param car
	 * 		the new "car" value
	 *
	 * @return the cons instance
	 */
	ConsStruct rplaca(final LispStruct car);

	/**
	 * Replaces the cdr of the cons with the provided object.
	 *
	 * @param cdr
	 * 		the new "cdr" value
	 *
	 * @return the cons instance
	 */
	ConsStruct rplacd(final LispStruct cdr);

	/**
	 * Returns a new ConsStruct with the provided "car" and "cdr" values.
	 *
	 * @param car
	 * 		the "car" of the cons
	 * @param cdr
	 * 		the "cdr" of the cons
	 *
	 * @return a new ConsStruct
	 */
	static ConsStruct toLispCons(final LispStruct car, final LispStruct cdr) {
		return new ConsStructImpl(car, cdr);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof final ConsStruct cons) {
			return car().equal(cons.car()) && cdr().equal(cons.cdr());
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof final ConsStruct cons) {
			return car().equalp(cons.car()) && cdr().equalp(cons.cdr());
		}
		return false;
	}
}
