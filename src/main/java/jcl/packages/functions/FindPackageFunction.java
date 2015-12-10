/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code find-package}.
 */
@Component
public final class FindPackageFunction extends AbstractPackageFunction {

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
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} package-designator object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} package-designator object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct packageDesignator = lispStructs[0];
		final PackageStruct aPackage = findPackage(packageDesignator);
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
