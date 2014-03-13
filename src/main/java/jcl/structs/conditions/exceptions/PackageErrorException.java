package jcl.structs.conditions.exceptions;

public class PackageErrorException extends ErrorException {

	private static final long serialVersionUID = 6077398599899378194L;

	public PackageErrorException(final String message) {
		super(message);
	}

	public PackageErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
