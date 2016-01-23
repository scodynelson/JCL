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
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-used-by-list}.
 */
@Component
public final class PackageUsedByListFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageUsedByListFunction() {
		super("Returns a list of other packages used by package.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} package object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} package object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code package-used-by-list} package function that returns the {@link
	 * PackageStruct#usedByList} as a {@link ListStruct}.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#usedByList} as a {@link ListStruct}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = validator.validatePackageDesignator(lispStruct, functionName());

		final List<PackageStruct> usedByList = aPackage.getUsedByList();
		return ListStruct.buildProperList(usedByList);
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
