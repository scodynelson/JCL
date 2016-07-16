package jcl.lang.condition.exception;

public class UndefinedFunctionException extends CellErrorException {

	private static final long serialVersionUID = -2386654941798877502L;

	public UndefinedFunctionException(final String message) {
		super(message);
	}

	public UndefinedFunctionException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
