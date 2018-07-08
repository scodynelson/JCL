/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.ArrayList;
import java.util.Collection;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.functions.FunctionHelpers;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-shadowing-symbols}.
 */
@Component
public final class PackageShadowingSymbolsFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "PACKAGE-SHADOWING-SYMBOLS";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageShadowingSymbolsFunction() {
		super("Returns a list of symbols that have been declared as shadowing symbols in package by shadow or shadowing-import.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-shadowing-symbols} package function that returns the {@link
	 * PackageStruct#getShadowingSymbols()} values as a {@link ListStruct}.
	 *
	 * @param arguments
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#getShadowingSymbols()} values as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = FunctionHelpers.asPackage(lispStruct);

		final Collection<SymbolStruct> shadowingSymbols = aPackage.getShadowingSymbols().values();
		return ListStruct.toLispList(new ArrayList<>(shadowingSymbols));
	}
}
