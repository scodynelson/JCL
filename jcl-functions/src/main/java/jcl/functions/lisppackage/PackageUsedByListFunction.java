/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.functions.FunctionHelpers;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-used-by-list}.
 */
@Component
public final class PackageUsedByListFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "PACKAGE-USED-BY-LIST";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageUsedByListFunction() {
		super("Returns a list of other packages used by package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-used-by-list} package function that returns the {@link
	 * PackageStruct#getUsedByList()} as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#getUsedByList()} as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = FunctionHelpers.asPackage(lispStruct);

		final List<PackageStruct> usedByList = aPackage.getUsedByList();
		return LispStructFactory.toProperList(usedByList);
	}
}
