package jcl.lang.condition.exception;

public class WarningException extends ConditionException {

	private static final long serialVersionUID = 9080827110048595957L;

	public WarningException(final String message) {
		super(message);
	}

	public WarningException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
