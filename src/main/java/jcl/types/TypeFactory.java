package jcl.types;

import jcl.LispType;

/**
 * This interface is the common parent for all lisp type factories.
 *
 * @param <TYPE> the type to get an instance of
 */
@FunctionalInterface
public interface TypeFactory<TYPE extends LispType> {

	/**
	 * This is a factory method to return an instance of the TYPE.
	 *
	 * @return an instance of the TYPE
	 */
	TYPE getInstance();
}
