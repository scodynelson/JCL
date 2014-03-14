package jcl.structs.conditions.exceptions;

import jcl.LispStruct;
import jcl.LispType;

public class ConditionException extends RuntimeException implements LispStruct {

	private static final long serialVersionUID = -6335987954040250984L;

	public ConditionException(final String message) {
		super(message);
	}

	public ConditionException(final String message, final Throwable cause) {
		super(message, cause);
	}

	@Override
	public LispType getType() {
		return null;
	}
}
