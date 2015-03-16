package jcl.compiler.old.exception;

public abstract class TOCException extends RuntimeException {

	private static final long serialVersionUID = -7231220457982555562L;

	Object tag;

	//static Stack<Object> catchTags = new Stack<Object>();

	/**
	 * This checks to see if the catch tag values match
	 * the current object's value with the object passed as a parameter.
	 */
	public boolean equals(final Object obj) {
		return (obj instanceof TOCException) && (((TOCException) obj).tag == tag);
	}

	/**
	 * This returns the current catch tag name.
	 * @return object
	 */
	public Object getTag() {
		return tag;
	}

	/**
	 * Overwrite the fillInStackTrace method for performance reasons.
	 */
	@Override
	public Throwable fillInStackTrace() {
		return null;
	}
}
