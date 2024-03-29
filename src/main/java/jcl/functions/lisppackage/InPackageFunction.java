/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

/**
 * System function implementation for {@code %in-package}.
 */
public final class InPackageFunction extends BuiltInFunctionStructImpl {

	private static final String NAME_ARGUMENT = "NAME";

	/**
	 * Public constructor passing the documentation string.
	 */
	public InPackageFunction() {
		super("Causes the the package named by name to become the current package.",
		      CommonLispSymbols.SYSTEM_IN_PACKAGE.getName(),
		      Parameters.forFunction(CommonLispSymbols.SYSTEM_IN_PACKAGE.getName())
		                .requiredParameter(NAME_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.SYSTEM_IN_PACKAGE;
	}

	/**
	 * {@inheritDoc} Application method for {@code in-package} package function that gets the package name to set the
	 * global {@code *package*} variable to. An {@link ErrorException} is thrown when the package cannot be found.
	 *
	 * @param arguments
	 * 		the function parameters
	 *
	 * @return the new {@link PackageStruct} value of the global {@code *package*} variable
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(NAME_ARGUMENT);
		final String name = StringStruct.fromDesignator(lispStruct).toJavaString();

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(name);
		CommonLispSymbols.PACKAGE_VAR.setfSymbolValue(newCurrentPackage);

		return newCurrentPackage;
	}
}
