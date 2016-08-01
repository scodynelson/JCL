package jcl.lang.condition.exception;

import jcl.lang.StreamStruct;

public class FileErrorException extends StreamErrorException {

	private static final long serialVersionUID = 7335690447447381411L;

	public FileErrorException(final String message, final StreamStruct streamWithError) {
		super(message, streamWithError);
	}

	public FileErrorException(final String message, final Throwable cause, final StreamStruct streamWithError) {
		super(message, cause, streamWithError);
	}
}
