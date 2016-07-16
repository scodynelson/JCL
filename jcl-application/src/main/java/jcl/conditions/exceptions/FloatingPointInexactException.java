package jcl.conditions.exceptions;

public class FloatingPointInexactException extends ArithmeticErrorException {

	private static final long serialVersionUID = 24915635430391115L;

	public FloatingPointInexactException(final String message) {
		super(message);
	}

	public FloatingPointInexactException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
