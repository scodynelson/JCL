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
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code delete-package}.
 */
@Component
public final class DeletePackageFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5106193902324547885L;

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

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final PackageStruct aPackage = findPackage(lispStruct);

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
