/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.GlobalPackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code use-package}.
 */
@Component
public final class UsePackageFunction extends AbstractPackageListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public UsePackageFunction() {
		super("Causes package to inherit all the external symbols of packages-to-use.",
		      "USE-PACKAGE"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#usePackage(PackageStruct...)} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#usePackage(PackageStruct...)} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, PackageStruct[]> packageListFunction() {
		return PackageStruct::usePackage;
	}

	/**
	 * {@inheritDoc}
	 * Performs validation on the provided {@link PackageStruct}s, ensuring that none of the {@link PackageStruct}s are
	 * equal to the constant {@link GlobalPackageStruct#KEYWORD} package.
	 *
	 * @param packageStructs
	 * 		the {@link PackageStruct}s to validate
	 */
	@Override
	protected void validatePackages(final PackageStruct... packageStructs) {
		for (final PackageStruct packageStruct : packageStructs) {
			if (GlobalPackageStruct.KEYWORD.eq(packageStruct)) {
				throw new ErrorException("Cannot use KEYWORD Package.");
			}
		}
	}
}
