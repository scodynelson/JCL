/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code make-package}.
 */
@Component
public final class MakePackageFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-PACKAGE";
	private static final String PACKAGE_NAME_ARGUMENT = "PACKAGE-NAME";

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
		final String packageName = lispStruct.asString().get().getAsJavaString();

		if (PackageStruct.findPackage(packageName) != null) {
			throw new ProgramErrorException("Package name " + packageName + " is already in use.");
		}

		final ListStruct nicknamesList = arguments.getKeyArgument(CommonLispSymbols.NICKNAMES_KEYWORD, ListStruct.class);
		final List<String> realNicknames
				= nicknamesList.stream()
				               .map(LispStruct::asString)
				               .map(Supplier::get)
				               .map(StringStruct::getAsJavaString)
				               .collect(Collectors.toList());

		final ListStruct usePackagesList = arguments.getKeyArgument(CommonLispSymbols.USE_KEYWORD, ListStruct.class);
		final List<PackageStruct> realUsePackages
				= usePackagesList.stream()
				                 .map(LispStruct::asPackage)
				                 .map(Supplier::get)
				                 .collect(Collectors.toList());

		return PackageStruct.valueOf(packageName, realNicknames, realUsePackages);
	}
}
