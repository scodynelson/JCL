package jcl.structs.conditions.exceptions;

public class FileErrorException extends StreamErrorException {

	private static final long serialVersionUID = 7335690447447381411L;

	public FileErrorException(final String message) {
		super(message);
	}

	public FileErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
