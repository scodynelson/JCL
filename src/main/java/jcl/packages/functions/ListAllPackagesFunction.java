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
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1723339238778095881L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public ListAllPackagesFunction() {
		super("Returns a fresh list of all registered packages.");
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final List<PackageStruct> allPackages = PackageStruct.listAllPackages();
		final LispStruct[] allPackagesArray = allPackages.toArray(new LispStruct[allPackages.size()]);
		return ListStruct.buildProperList(allPackagesArray);
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
