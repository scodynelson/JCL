package jcl.lang.condition.exception;

public class SimpleWarningException extends WarningException {
	// TODO: also extends SimpleConditionException

	private static final long serialVersionUID = 7284934065394054738L;

	public SimpleWarningException(final String message) {
		super(message);
	}

	public SimpleWarningException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
