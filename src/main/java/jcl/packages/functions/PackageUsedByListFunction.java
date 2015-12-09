/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-used-by-list}.
 */
@Component
public final class PackageUsedByListFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2137341320981943787L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageUsedByListFunction() {
		super("Returns a list of other packages used by package.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = findPackage(lispStruct);

		final List<PackageStruct> usedByList = aPackage.getUsedByList();
		final LispStruct[] usedByListArray = usedByList.toArray(new LispStruct[usedByList.size()]);
		return ListStruct.buildProperList(usedByListArray);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-used-by-list} as a string.
	 *
	 * @return the function name {@code package-used-by-list} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-USED-BY-LIST";
	}
}
