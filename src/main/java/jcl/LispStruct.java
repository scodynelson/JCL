package jcl;

/**
 * The {@link LispStruct} is the representation for all Lisp types.
 */
public interface LispStruct {

	/**
	 * This method returns the type of the struct.
	 *
	 * @return the type of the struct
	 */
	LispType getType();

	/**
	 * This method returns the printed representation of the struct.
	 *
	 * @return the printed representation of the struct
	 */
	String printStruct();
}
