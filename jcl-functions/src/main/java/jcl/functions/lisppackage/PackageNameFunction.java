/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.lang.LispStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-name}.
 */
@Component
public final class PackageNameFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PACKAGE-NAME";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageNameFunction() {
		super("Returns the string that names package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-name} package function that returns the {@link PackageStructImpl#name} as a
	 * {@link StringStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStructImpl#name} as a {@link StringStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStructImpl aPackage = lispStruct.asPackage().get();

		final String name = aPackage.getName();
		return (name == null) ? NILStruct.INSTANCE : LispStructFactory.toString(name);
	}
}
