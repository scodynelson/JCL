/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code make-package}.
 */
@Component
public final class MakePackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-PACKAGE";
	private static final String PACKAGE_NAME_ARGUMENT = "PACKAGE-NAME";

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public MakePackageFunction() {
		super("Creates a new package with the name package-name.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_NAME_ARGUMENT)
		                .keyParameter(CommonLispSymbols.NICKNAMES_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.USE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the package function that creates a new {@link PackageStruct} object with the provided
	 * string-designator package name and the optional nicknames list and packages to use list.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the newly created {@link PackageStruct} object
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_NAME_ARGUMENT);
		final String packageName = validator.validateStringDesignator(lispStruct, functionName, "Package Name");

		if (PackageStruct.findPackage(packageName) != null) {
			throw new ProgramErrorException("Package name " + packageName + " is already in use.");
		}

		final ListStruct nicknamesList = arguments.getKeyArgument(CommonLispSymbols.NICKNAMES_KEYWORD, ListStruct.class);
		final List<String> realNicknames
				= nicknamesList.stream()
				               .map(e -> validator.validateStringDesignator(e, functionName, "Nickname"))
				               .collect(Collectors.toList());

		final ListStruct usePackagesList = arguments.getKeyArgument(CommonLispSymbols.USE_KEYWORD, ListStruct.class);
		final List<PackageStruct> realUsePackages
				= usePackagesList.stream()
				                 .map(e -> validator.validatePackageDesignator(e, functionName))
				                 .collect(Collectors.toList());

		return new PackageStruct(packageName, realNicknames, realUsePackages);
	}
}
