/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code list-all-packages}.
 */
@Component
public final class ListAllPackagesFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Public constructor passing the documentation string.
	 */
	public ListAllPackagesFunction() {
		super("Returns a fresh list of all registered packages.");
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code list-all-packages} package function returns the result of {@link
	 * PackageStruct#listAllPackages()} as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of {@link PackageStruct#listAllPackages()} as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final List<PackageStruct> allPackages = PackageStruct.listAllPackages();
		return ListStruct.buildProperList(allPackages);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code list-all-packages} as a string.
	 *
	 * @return the function name {@code list-all-packages} as a string
	 */
	@Override
	protected String functionName() {
		return "LIST-ALL-PACKAGES";
	}
}
