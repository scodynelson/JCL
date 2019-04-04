/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.PackageVariables;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code in-package}.
 */
@Component
public final class InPackageFunction extends CommonLispBuiltInFunctionStructBase {

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
	 * {@inheritDoc}
	 * Application method for {@code in-package} package function that gets the package name to set the global {@code
	 * *package*} variable to. An {@link ErrorException} is thrown when the package cannot be found.
	 *
	 * @param arguments
	 * 		the function parameters
	 *
	 * @return the new {@link PackageStruct} value of the global {@code *package*} variable
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(NAME_ARGUMENT);
		final String name = StringStruct.toLispString(lispStruct).toJavaString();

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(name);
		if (newCurrentPackage == null) {
			throw new ErrorException("There is no package named " + name);
		}

		PackageVariables.PACKAGE.setValue(newCurrentPackage);

		return newCurrentPackage;
	}
}
