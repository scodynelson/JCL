package jcl.lang.condition.exception;

import jcl.lang.stream.LispStream;

public class EndOfFileException extends StreamErrorException {

	private static final long serialVersionUID = -5697780763099837337L;

	public EndOfFileException(final String message, final LispStream streamWithError) {
		super(message, streamWithError);
	}

	public EndOfFileException(final String message, final Throwable cause, final LispStream streamWithError) {
		super(message, cause, streamWithError);
	}
}
