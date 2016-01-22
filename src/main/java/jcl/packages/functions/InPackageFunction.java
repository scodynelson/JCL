/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code in-package}.
 */
@Component
public final class InPackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5831829564256951336L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public InPackageFunction() {
		super("Causes the the package named by name to become the current package.");
	}

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} string-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} string-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code in-package} package function that gets the package name to set the global {@code
	 * *package*} variable to. An {@link ErrorException} is thrown when the package cannot be found.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the new {@link PackageStruct} value of the global {@code *package*} variable
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String name = validator.validateStringDesignator(lispStruct, functionName(), "Package Name");

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(name);
		if (newCurrentPackage == null) {
			throw new ErrorException("There is no package named " + name);
		}

		PackageVariables.PACKAGE.setValue(newCurrentPackage);

		return newCurrentPackage;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code in-package} as a string.
	 *
	 * @return the function name {@code in-package} as a string
	 */
	@Override
	protected String functionName() {
		return "IN-PACKAGE";
	}
}
