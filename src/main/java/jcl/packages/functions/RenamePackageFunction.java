/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code rename-package}.
 */
@Component
public final class RenamePackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RENAME-PACKAGE";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";
	private static final String NEW_NAME_ARGUMENT = "NEW-NAME";
	private static final String NEW_NICKNAMES_ARGUMENT = "NEW-NICKNAMES";

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public RenamePackageFunction() {
		super("Replaces the name and nicknames of package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		                .requiredParameter(NEW_NAME_ARGUMENT)
		                .optionalParameter(NEW_NICKNAMES_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the package function that renames the provided {@link PackageStruct} package-designator,
	 * optionally giving it the provided nicknames.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the renamed {@link PackageStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final PackageStruct aPackage = validator.validatePackageDesignator(arguments.getRequiredArgument(PACKAGE_ARGUMENT), functionName);
		final String newName = validator.validatePackageDesignatorAsString(arguments.getRequiredArgument(NEW_NAME_ARGUMENT), functionName, "New Name");

		if (arguments.hasOptionalArgument(NEW_NICKNAMES_ARGUMENT)) {
			final ListStruct newNicknamesList = arguments.getOptionalArgument(NEW_NICKNAMES_ARGUMENT, ListStruct.class);
			final List<String> newNicknames
					= newNicknamesList.stream()
					                  .map(newNickname -> validator.validateStringDesignator(newNickname, functionName, "New Nickname"))
					                  .collect(Collectors.toList());
			aPackage.renamePackage(newName, newNicknames);
		} else {
			aPackage.renamePackage(newName);
		}

		return aPackage;
	}
}
