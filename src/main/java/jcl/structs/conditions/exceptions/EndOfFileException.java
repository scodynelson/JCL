package jcl.structs.conditions.exceptions;

public class EndOfFileException extends StreamErrorException {

	private static final long serialVersionUID = -5697780763099837337L;

	public EndOfFileException(final String message) {
		super(message);
	}

	public EndOfFileException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
