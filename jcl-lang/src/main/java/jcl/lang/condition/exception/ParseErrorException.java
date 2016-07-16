package jcl.lang.condition.exception;

public class ParseErrorException extends ErrorException {

	private static final long serialVersionUID = -6219328394839781323L;

	public ParseErrorException(final String message) {
		super(message);
	}

	public ParseErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
