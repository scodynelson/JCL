/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.List;
import java.util.stream.Collectors;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
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
