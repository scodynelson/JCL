package jcl.lang.condition.exception;

import jcl.lang.StreamStruct;

public class StreamErrorException extends ErrorException {

	private static final long serialVersionUID = -3552853955037901071L;

	private final StreamStruct streamWithError;

	public StreamErrorException(final String message, final StreamStruct streamWithError) {
		super(message);
		this.streamWithError = streamWithError;
	}

	public StreamErrorException(final String message, final Throwable cause, final StreamStruct streamWithError) {
		super(message, cause);
		this.streamWithError = streamWithError;
	}

	public StreamStruct getStreamWithError() {
		return streamWithError;
	}
}
