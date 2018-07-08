/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.functions.FunctionHelpers;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-package}.
 */
@Component
public final class FindPackageFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FIND-PACKAGE";
	private static final String NAME_ARGUMENT = "NAME";

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindPackageFunction() {
		super("Locates and returns the package whose name or nickname is name.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NAME_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code find-package} package function that returns the {@link PackageStruct} with the
	 * provided package-designator parameter.
	 *
	 * @param arguments
	 * 		the function parameters
	 *
	 * @return {@link PackageStruct} with the provided package-designator parameter
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct packageDesignator = arguments.getRequiredArgument(NAME_ARGUMENT);
		final PackageStruct aPackage = FunctionHelpers.asPackage(packageDesignator);
		return (aPackage == null) ? NILStruct.INSTANCE : aPackage;
	}
}
