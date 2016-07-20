/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.ArrayList;
import java.util.Collection;

import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-shadowing-symbols}.
 */
@Component
public final class PackageShadowingSymbolsFunction extends CommonLispBuiltInFunctionStruct {

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
	 * PackageStruct#shadowingSymbols} values as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#shadowingSymbols} values as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = lispStruct.asPackage().get();

		final Collection<SymbolStruct> shadowingSymbols = aPackage.getShadowingSymbols().values();
		return ListStruct.buildProperList(new ArrayList<>(shadowingSymbols));
	}
}
