package jcl.lang.condition.exception;

import jcl.lang.PackageStruct;

public class PackageErrorException extends ErrorException {

	private static final long serialVersionUID = 6077398599899378194L;

	@SuppressWarnings("TransientFieldNotInitialized")
	private final transient PackageStruct packageWithError;

	public PackageErrorException(final String message, final PackageStruct packageWithError) {
		super(message);
		this.packageWithError = packageWithError;
	}

	public PackageErrorException(final String message, final Throwable cause, final PackageStruct packageWithError) {
		super(message, cause);
		this.packageWithError = packageWithError;
	}

	public PackageStruct getPackageWithError() {
		return packageWithError;
	}
}
