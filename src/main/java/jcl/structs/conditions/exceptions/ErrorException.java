package jcl.structs.conditions.exceptions;

public class ErrorException extends SeriousConditionException {

	private static final long serialVersionUID = 8682106592433127310L;

	public ErrorException(final String message) {
		super(message);
	}

	public ErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
