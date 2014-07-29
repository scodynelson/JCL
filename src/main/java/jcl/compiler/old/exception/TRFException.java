package jcl.compiler.old.exception;

public abstract class TRFException extends TOCException {

	private static final long serialVersionUID = -9771800531338784L;
	Object value;

	//static Stack<Object> catchTags = new Stack<Object>();

	/**
	 * This returns the result-form that is returned with the transfer of control
	 */
	public Object getValue() {
		return value;
	}
}



