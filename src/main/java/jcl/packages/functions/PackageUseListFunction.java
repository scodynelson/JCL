/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class PackageUseListFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5728008797234255860L;

	@Autowired
	private FindPackageFunction findPackageFunction;

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
		final PackageStruct aPackage = findPackageFunction.findPackage(lispStruct);

		final List<PackageStruct> useList = aPackage.getUseList();
		final LispStruct[] useListArray = useList.toArray(new LispStruct[useList.size()]);
		return ListStruct.buildProperList(useListArray);
	}

	@Override
	protected String functionName() {
		return "PACKAGE-USE-LIST";
	}
}
