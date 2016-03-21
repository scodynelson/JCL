/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-nicknames}.
 */
@Component
public final class PackageNicknamesFunction extends CommonLispBuiltInFunctionStruct {

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
	 * PackageStruct#nicknames} as a {@link ListStruct} of {@link StringStruct}s.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#nicknames} as a {@link ListStruct} of {@link StringStruct}s
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getRequiredArgument(PACKAGE_ARGUMENT);
		final PackageStruct aPackage = lispStruct.asPackage().get();

		final List<String> nicknames = aPackage.getNicknames();
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(StringStruct::new)
				         .collect(Collectors.toList());
		return ListStruct.buildProperList(nicknamesStructs);
	}
}
