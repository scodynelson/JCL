package jcl.structs.conditions.exceptions;

public class UnboundVariableException extends CellErrorException {

	private static final long serialVersionUID = -9211182423046551952L;

	public UnboundVariableException(final String message) {
		super(message);
	}

	public UnboundVariableException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
