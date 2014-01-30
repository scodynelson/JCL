package jcl.structs.conditions.exceptions;

public class DivisionByZeroException extends ArithmeticErrorException {

	private static final long serialVersionUID = -3843807728771812699L;

	public DivisionByZeroException(final String message) {
		super(message);
	}

	public DivisionByZeroException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
