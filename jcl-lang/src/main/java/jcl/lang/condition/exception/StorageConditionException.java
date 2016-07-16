package jcl.lang.condition.exception;

public class StorageConditionException extends SeriousConditionException {

	private static final long serialVersionUID = 2928094272357383125L;

	public StorageConditionException(final String message) {
		super(message);
	}

	public StorageConditionException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
