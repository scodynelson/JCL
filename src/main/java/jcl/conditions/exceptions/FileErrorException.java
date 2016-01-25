package jcl.conditions.exceptions;

import jcl.streams.LispStream;

public class FileErrorException extends StreamErrorException {

	private static final long serialVersionUID = 7335690447447381411L;

	public FileErrorException(final String message, final LispStream streamWithError) {
		super(message, streamWithError);
	}

	public FileErrorException(final String message, final Throwable cause, final LispStream streamWithError) {
		super(message, cause, streamWithError);
	}
}
