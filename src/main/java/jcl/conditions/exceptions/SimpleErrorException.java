package jcl.conditions.exceptions;

public class SimpleErrorException extends ErrorException {
	// TODO: also extends SimpleConditionException

	private static final long serialVersionUID = 3403667730979109011L;

	public SimpleErrorException(final String message) {
		super(message);
	}

	public SimpleErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
