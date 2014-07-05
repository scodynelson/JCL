package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;

/**
 * Implements the Common Lisp function CONS.  The CONS of two objects will
 * return a list whose CAR is the first object and whose CDR is the second object.
 */
public class ConsFunction {
	public static final ConsFunction FUNCTION = new ConsFunction();

	public ConsStruct funcall(LispStruct arg1, LispStruct arg2) {
		return new ConsStruct(arg1, arg2);
	}

}

