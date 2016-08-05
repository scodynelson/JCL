/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-use-list}.
 */
@Component
public final class PackageUseListFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PACKAGE-USE-LIST";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageUseListFunction() {
		super("Returns a list of other packages that use package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-use-list} package function that returns the {@link PackageStructImpl#useList}
	 * as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStructImpl#useList} as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStructImpl aPackage = lispStruct.asPackage().get();

		final List<PackageStructImpl> useList = aPackage.getUseList();
		return LispStructFactory.toProperList(useList);
	}
}
