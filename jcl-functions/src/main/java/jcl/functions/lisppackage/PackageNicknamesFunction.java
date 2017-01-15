/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;
import java.util.stream.Collectors;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.functions.FunctionHelpers;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-nicknames}.
 */
@Component
public final class PackageNicknamesFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "PACKAGE-NICKNAMES";
	private static final String PACKAGE_ARGUMENT = "PACKAGE";

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageNicknamesFunction() {
		super("Returns the list of nickname strings for package, not including the name of package.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(PACKAGE_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-nicknames} package function that returns the {@link
	 * PackageStruct#getNicknames()} as a {@link ListStruct} of {@link StringStruct}s.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#getNicknames()} as a {@link ListStruct} of {@link StringStruct}s
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = FunctionHelpers.asPackage(lispStruct);

		final List<String> nicknames = aPackage.getNicknames();
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(LispStructFactory::toString)
				         .collect(Collectors.toList());
		return LispStructFactory.toProperList(nicknamesStructs);
	}
}
