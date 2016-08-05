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
 * Function implementation for {@code list-all-packages}.
 */
@Component
public final class ListAllPackagesFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LIST-ALL-PACKAGES";

	/**
	 * Public constructor passing the documentation string.
	 */
	public ListAllPackagesFunction() {
		super("Returns a fresh list of all registered packages.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code list-all-packages} package function returns the result of {@link
	 * PackageStructImpl#listAllPackages()} as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of {@link PackageStructImpl#listAllPackages()} as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<PackageStructImpl> allPackages = PackageStructImpl.listAllPackages();
		return LispStructFactory.toProperList(allPackages);
	}
}
