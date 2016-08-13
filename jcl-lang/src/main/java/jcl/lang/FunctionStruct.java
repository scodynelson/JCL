package jcl.lang;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public interface FunctionStruct extends LispStruct {

	LispStruct apply(LispStruct... lispStructs);
}
