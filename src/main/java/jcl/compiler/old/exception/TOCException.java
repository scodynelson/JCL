package jcl.compiler.old.exception;

public abstract class TOCException extends RuntimeException {

	private static final long serialVersionUID = -7231220457982555562L;
	Object tag;

	//static Stack<Object> catchTags = new Stack<Object>();

	/**
	 * This checks to see if the catch tag values match
	 * the current object's value with the object passed as a parameter.
	 */
	public boolean equals(Object obj) {
		return (obj instanceof TOCException) &&
				(((TOCException) obj).getTag() == tag);
	}

	/**
	 * This returns the current catch tag name
	 */
	public Object getTag() {
		return tag;
	}

	/**
	 * Overwrite the fillInStackTrace method for performance reasons
	 */
	public Throwable fillInStackTrace() {
		return null;
	}
}
