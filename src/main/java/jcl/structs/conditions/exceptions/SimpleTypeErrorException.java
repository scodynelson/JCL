package jcl.structs.conditions.exceptions;

public class SimpleTypeErrorException extends TypeErrorException {
	// TODO: also extends SimpleConditionException

	private static final long serialVersionUID = -2425117221665919981L;

	public SimpleTypeErrorException(final String message) {
		super(message);
	}

	public SimpleTypeErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
