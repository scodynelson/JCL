/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import jcl.LispStruct;
import jcl.conditions.exceptions.PackageErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-error-package}.
 */
@Component
public final class PackageErrorPackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PACKAGE-ERROR-PACKAGE";
	private static final String CONDITION_ARGUMENT = "CONDITION";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageErrorPackageFunction() {
		super("Returns a designator for the offending package in the situation represented by the condition.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CONDITION_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-error-package} package function that returns the {@link PackageStruct}
	 * that was a part of provided {@link PackageErrorException} condition.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct} that was a part of provided {@link PackageErrorException} condition
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final PackageErrorException packageErrorException = arguments.getRequiredArgument(CONDITION_ARGUMENT, PackageErrorException.class);
		return packageErrorException.getPackageWithError();
	}
}
