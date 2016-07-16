package jcl.conditions.exceptions;

public class FloatingPointUnderflowException extends ArithmeticErrorException {

	private static final long serialVersionUID = -8268245559312751532L;

	public FloatingPointUnderflowException(final String message) {
		super(message);
	}

	public FloatingPointUnderflowException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
