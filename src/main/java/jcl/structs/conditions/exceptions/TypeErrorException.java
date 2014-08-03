package jcl.structs.conditions.exceptions;

public class TypeErrorException extends ErrorException {

	private static final long serialVersionUID = -3738541431979254592L;

	public TypeErrorException(final String message) {
		super(message);
	}

	public TypeErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
