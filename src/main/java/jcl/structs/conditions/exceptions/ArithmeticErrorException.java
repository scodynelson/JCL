package jcl.structs.conditions.exceptions;

public class ArithmeticErrorException extends ErrorException {

	private static final long serialVersionUID = -8960833115412371525L;

	public ArithmeticErrorException(final String message) {
		super(message);
	}

	public ArithmeticErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
