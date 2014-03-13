package jcl.structs.conditions.exceptions;

public class CellErrorException extends ErrorException {

	private static final long serialVersionUID = -3117741772702019185L;

	public CellErrorException(final String message) {
		super(message);
	}

	public CellErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
