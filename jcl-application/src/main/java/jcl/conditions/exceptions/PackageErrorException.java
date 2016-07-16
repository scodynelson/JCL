package jcl.conditions.exceptions;

import jcl.LispType;
import jcl.conditions.ConditionType;
import jcl.packages.PackageStruct;

public class PackageErrorException extends ErrorException {

	private static final long serialVersionUID = 6077398599899378194L;

	private final PackageStruct packageWithError;

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

	@Override
	public LispType getType() {
		return ConditionType.PACKAGE_ERROR;
	}
}
