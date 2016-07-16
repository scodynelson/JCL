package jcl.conditions.exceptions;

import jcl.LispType;
import jcl.conditions.ConditionType;
import jcl.streams.LispStream;

public class StreamErrorException extends ErrorException {

	private static final long serialVersionUID = -3552853955037901071L;

	private final LispStream streamWithError;

	public StreamErrorException(final String message, final LispStream streamWithError) {
		super(message);
		this.streamWithError = streamWithError;
	}

	public StreamErrorException(final String message, final Throwable cause, final LispStream streamWithError) {
		super(message, cause);
		this.streamWithError = streamWithError;
	}

	public LispStream getStreamWithError() {
		return streamWithError;
	}

	@Override
	public LispType getType() {
		return ConditionType.STREAM_ERROR;
	}
}
