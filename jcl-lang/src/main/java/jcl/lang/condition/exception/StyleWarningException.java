package jcl.lang.condition.exception;

public class StyleWarningException extends WarningException {

	private static final long serialVersionUID = -817335836636501684L;

	public StyleWarningException(final String message) {
		super(message);
	}

	public StyleWarningException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
