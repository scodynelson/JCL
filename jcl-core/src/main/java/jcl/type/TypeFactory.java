/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

/**
 * This interface is the common parent for all lisp type factories.
 *
 * @param <TYPE>
 * 		the type to get an instance of
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