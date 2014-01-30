package jcl.structs.conditions.exceptions;

import jcl.structs.LispStruct;
import jcl.types.LispType;

public class ConditionException extends Exception implements LispStruct {

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
