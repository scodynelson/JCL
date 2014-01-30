package jcl.structs.conditions.exceptions;

public class PrintNotReadableException extends ErrorException {

	private static final long serialVersionUID = 1748713507147848769L;

	public PrintNotReadableException(final String message) {
		super(message);
	}

	public PrintNotReadableException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
