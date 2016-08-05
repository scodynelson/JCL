package jcl.lang.condition.exception;

import jcl.lang.PackageStructImpl;
import jcl.lang.condition.ConditionType;
import jcl.type.LispType;

public class PackageErrorException extends ErrorException {

	private static final long serialVersionUID = 6077398599899378194L;

	private final PackageStructImpl packageWithError;

	public PackageErrorException(final String message, final PackageStructImpl packageWithError) {
		super(message);
		this.packageWithError = packageWithError;
	}

	public PackageErrorException(final String message, final Throwable cause, final PackageStructImpl packageWithError) {
		super(message, cause);
		this.packageWithError = packageWithError;
	}

	public PackageStructImpl getPackageWithError() {
		return packageWithError;
	}

	@Override
	public LispType getType() {
		return ConditionType.PACKAGE_ERROR;
	}
}
