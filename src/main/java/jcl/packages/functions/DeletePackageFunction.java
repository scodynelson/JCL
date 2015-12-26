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
import jcl.symbols.TStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code delete-package}.
 */
@Component
public final class DeletePackageFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5106193902324547885L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Public constructor passing the documentation string.
	 */
	public DeletePackageFunction() {
		super("Deletes package from all package system data structures. If the operation is successful, returns true, otherwise nil.");
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} package object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} package object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code delete-package} package function that deletes the provided {@link PackageStruct}
	 * package-designator.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return {@link TStruct#INSTANCE}
	 */
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = validator.validatePackageDesignator(lispStruct, functionName());

		if (aPackage.getName() == null) {
			return NullStruct.INSTANCE;
		}

		aPackage.deletePackage();
		return TStruct.INSTANCE;
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code delete-package} as a string.
	 *
	 * @return the function name {@code delete-package} as a string
	 */
	@Override
	protected String functionName() {
		return "DELETE-PACKAGE";
	}
}
