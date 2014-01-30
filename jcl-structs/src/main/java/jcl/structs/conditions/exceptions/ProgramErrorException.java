package jcl.structs.conditions.exceptions;

public class ProgramErrorException extends ErrorException {

	private static final long serialVersionUID = 2893176539426307669L;

	public ProgramErrorException(final String message) {
		super(message);
	}

	public ProgramErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
