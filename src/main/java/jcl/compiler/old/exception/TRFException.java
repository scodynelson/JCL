package jcl.compiler.old.exception;

public abstract class TRFException extends TOCException {

	Object value;

	//static Stack<Object> catchTags = new Stack<Object>();

	/**
	 * This returns the result-form that is returned with the transfer of control
	 */
	public Object getValue() {
		return value;
	}
}



