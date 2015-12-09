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
 * Function implementation for {@code package-use-list}.
 */
@Component
public final class PackageUseListFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5728008797234255860L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageUseListFunction() {
		super("Returns a list of other packages that use package.");
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

		final List<PackageStruct> useList = aPackage.getUseList();
		final LispStruct[] useListArray = useList.toArray(new LispStruct[useList.size()]);
		return ListStruct.buildProperList(useListArray);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-use-list} as a string.
	 *
	 * @return the function name {@code package-use-list} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-USE-LIST";
	}
}
