package jcl.structs.conditions.exceptions;

public class ControlErrorException extends ErrorException {

	private static final long serialVersionUID = 9023889261854667713L;

	public ControlErrorException(final String message) {
		super(message);
	}

	public ControlErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
