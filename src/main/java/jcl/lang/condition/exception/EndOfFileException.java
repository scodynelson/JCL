package jcl.lang.condition.exception;

import jcl.lang.StreamStruct;

public class EndOfFileException extends StreamErrorException {

	public static final String END_OF_FILE_REACHED = "End of file reached.";

	private static final long serialVersionUID = -5697780763099837337L;

	public EndOfFileException(final StreamStruct streamWithError) {
		super(END_OF_FILE_REACHED, streamWithError);
	}

	public EndOfFileException(final Throwable cause, final StreamStruct streamWithError) {
		super(END_OF_FILE_REACHED, cause, streamWithError);
	}
}
