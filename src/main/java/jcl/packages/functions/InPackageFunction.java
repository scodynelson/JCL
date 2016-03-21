/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code in-package}.
 */
@Component
public final class InPackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "IN-PACKAGE";
	private static final String NAME_ARGUMENT = "NAME";

	/**
	 * Public constructor passing the documentation string.
	 */
	public InPackageFunction() {
		super("Causes the the package named by name to become the current package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NAME_ARGUMENT)
		);
	}

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * {@inheritDoc}
	 * Application method for {@code in-package} package function that gets the package name to set the global {@code
	 * *package*} variable to. An {@link ErrorException} is thrown when the package cannot be found.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the new {@link PackageStruct} value of the global {@code *package*} variable
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(NAME_ARGUMENT);
		final String name = validator.validateStringDesignator(lispStruct, functionName, "Package Name");

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(name);
		if (newCurrentPackage == null) {
			throw new ErrorException("There is no package named " + name);
		}

		PackageVariables.PACKAGE.setValue(newCurrentPackage);

		return newCurrentPackage;
	}
}
