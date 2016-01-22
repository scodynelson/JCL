/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-package}.
 */
@Component
public final class FindPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3051378531665513323L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public FindPackageFunction() {
		super("Locates and returns the package whose name or nickname is name.");
	}

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} package-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} package-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code find-package} package function that returns the {@link PackageStruct} with the
	 * provided package-designator parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link PackageStruct} with the provided package-designator parameter
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct packageDesignator = lispStructs[0];
		final PackageStruct aPackage = validator.validatePackageDesignator(packageDesignator, functionName());
		return (aPackage == null) ? NullStruct.INSTANCE : aPackage;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code find-package} as a string.
	 *
	 * @return the function name {@code find-package} as a string
	 */
	@Override
	protected String functionName() {
		return "FIND-PACKAGE";
	}
}
