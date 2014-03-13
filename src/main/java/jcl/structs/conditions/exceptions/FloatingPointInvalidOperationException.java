package jcl.structs.conditions.exceptions;

public class FloatingPointInvalidOperationException extends ArithmeticErrorException {

	private static final long serialVersionUID = 986715515528096317L;

	public FloatingPointInvalidOperationException(final String message) {
		super(message);
	}

	public FloatingPointInvalidOperationException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
