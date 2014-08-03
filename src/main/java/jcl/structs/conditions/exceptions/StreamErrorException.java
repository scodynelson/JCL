package jcl.structs.conditions.exceptions;

public class StreamErrorException extends ErrorException {

	private static final long serialVersionUID = -3552853955037901071L;

	public StreamErrorException(final String message) {
		super(message);
	}

	public StreamErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
