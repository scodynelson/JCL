package jcl.structs.conditions.exceptions;

public class FloatingPointOverflowException extends ArithmeticErrorException {

	private static final long serialVersionUID = -8765395832568542058L;

	public FloatingPointOverflowException(final String message) {
		super(message);
	}

	public FloatingPointOverflowException(final String message, final Throwable cause) {
		super(message, cause);
	}
}