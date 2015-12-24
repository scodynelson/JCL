/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;

abstract class AbstractOptionalPackageFunction extends AbstractPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1300235783486543518L;

	protected AbstractOptionalPackageFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PACKAGE")
				.initForm(PackageVariables.PACKAGE.getVariableValue())
				.suppliedPBinding()
				.buildList();
	}

	protected PackageStruct getPackage(final LispStruct... lispStructs) {
		final PackageStruct aPackage;
		if (lispStructs.length >= 2) {
			final LispStruct packageDesignator = lispStructs[1];
			aPackage = findPackage(packageDesignator);
		} else {
			aPackage = PackageVariables.PACKAGE.getVariableValue();
		}
		return aPackage;
	}
}
