/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code package-nicknames}.
 */
@Component
public final class PackageNicknamesFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public PackageNicknamesFunction() {
		super("Returns the list of nickname strings for package, not including the name of package.");
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
	 * Application method for {@code package-nicknames} package function that returns the {@link
	 * PackageStruct#nicknames} as a {@link ListStruct} of {@link StringStruct}s.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link PackageStruct#nicknames} as a {@link ListStruct} of {@link StringStruct}s
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = validator.validatePackageDesignator(lispStruct, functionName());

		final List<String> nicknames = aPackage.getNicknames();
		final List<LispStruct> nicknamesStructs =
				nicknames.stream()
				         .map(StringStruct::new)
				         .collect(Collectors.toList());
		return ListStruct.buildProperList(nicknamesStructs);
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code package-nicknames} as a string.
	 *
	 * @return the function name {@code package-nicknames} as a string
	 */
	@Override
	protected String functionName() {
		return "PACKAGE-NICKNAMES";
	}
}
