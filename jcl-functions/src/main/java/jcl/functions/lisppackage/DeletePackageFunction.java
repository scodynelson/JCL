/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.TStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code delete-package}.
 */
@Component
public final class DeletePackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "DELETE-PACKAGE";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public DeletePackageFunction() {
		super("Deletes package from all package system data structures. If the operation is successful, returns true, otherwise nil.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code delete-package} package function that deletes the provided {@link PackageStruct}
	 * package-designator.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = lispStruct.asPackage().get();

		if (aPackage.getName() == null) {
			return NILStruct.INSTANCE;
		}

		aPackage.deletePackage();
		return TStruct.INSTANCE;
	}
}
