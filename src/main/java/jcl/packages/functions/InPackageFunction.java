/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code in-package}.
 */
@Component
public final class InPackageFunction extends AbstractPackageFunction {

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
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} string-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} string-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final String name = getStringFromStringDesignator(lispStruct, "Package Name");

		final PackageStruct newCurrentPackage = PackageStruct.findPackage(name);
		if (newCurrentPackage == null) {
			throw new ReaderErrorException("There is no package named " + name);
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
