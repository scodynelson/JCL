package jcl.lang.condition.exception;

import jcl.lang.LispStruct;

public class ConditionException extends RuntimeException implements LispStruct {

	private static final long serialVersionUID = -6335987954040250984L;

	public ConditionException(final String message) {
		super(message);
	}

	public ConditionException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
