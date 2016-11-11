package jcl.lang.condition.exception;

public class SeriousConditionException extends ConditionException {

	private static final long serialVersionUID = 8547863340938900596L;

	public SeriousConditionException(final String message) {
		super(message);
	}

	public SeriousConditionException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
