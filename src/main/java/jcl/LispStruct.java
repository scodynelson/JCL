package jcl;

/**
 * The {@code LispStruct} is the representation for all Lisp types.
 */
public interface LispStruct {

	/**
	 * This method returns the type of the struct.
	 *
	 * @return the type of the struct
	 */
	LispType getType();
}
