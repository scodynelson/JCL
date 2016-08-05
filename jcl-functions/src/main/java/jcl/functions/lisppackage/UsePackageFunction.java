/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.lisppackage;

import java.util.function.BiConsumer;

import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.condition.exception.ErrorException;
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
	 * Returns {@link PackageStructImpl#usePackage(PackageStructImpl...)} as a method reference function.
	 *
	 * @return returns {@link PackageStructImpl#usePackage(PackageStructImpl...)} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStructImpl, PackageStructImpl[]> packageListFunction() {
		return PackageStructImpl::usePackage;
	}

	/**
	 * {@inheritDoc}
	 * Performs validation on the provided {@link PackageStructImpl}s, ensuring that none of the {@link PackageStructImpl}s are
	 * equal to the constant {@link GlobalPackageStruct#KEYWORD} package.
	 *
	 * @param packageStructs
	 * 		the {@link PackageStructImpl}s to validate
	 */
	@Override
	protected void validatePackages(final PackageStructImpl... packageStructs) {
		for (final PackageStructImpl packageStruct : packageStructs) {
			if (GlobalPackageStruct.KEYWORD.equals(packageStruct)) {
				throw new ErrorException("Cannot use KEYWORD Package.");
			}
		}
	}
}
