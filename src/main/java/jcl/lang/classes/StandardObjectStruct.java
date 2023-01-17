package jcl.lang.classes;

import jcl.lang.internal.LispStructImpl;

/**
 * The {@link StandardObjectStruct} is the object representation of a Lisp 'standard-object' type.
 */
public abstract class StandardObjectStruct extends LispStructImpl {

	@Override
	public String toString() {
		final String typeClassName = getClass().getSimpleName();
		return "#<" + typeClassName + '@' + hashCode() + '>';
	}
}
