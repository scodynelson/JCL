package jcl.structs.conditions.exceptions;

public class SimpleConditionException extends ConditionException {

	private static final long serialVersionUID = -7792813146253799938L;

	public SimpleConditionException(final String message) {
		super(message);
	}

	public SimpleConditionException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
