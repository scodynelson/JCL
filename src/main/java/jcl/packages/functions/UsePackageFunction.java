/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.conditions.exceptions.ErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code use-package}.
 */
@Component
public final class UsePackageFunction extends AbstractPackageListPackageFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2314927832245574872L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UsePackageFunction() {
		super("Causes package to inherit all the external symbols of packages-to-use.");
	}

	@Override
	protected BiConsumer<PackageStruct, PackageStruct[]> packageListFunction() {
		return PackageStruct::usePackage;
	}

	@Override
	protected void validatePackages(final PackageStruct... packageStructs) {
		for (final PackageStruct packageStruct : packageStructs) {
			if (GlobalPackageStruct.KEYWORD.equals(packageStruct)) {
				throw new ErrorException("Cannot use KEYWORD Package.");
			}
		}
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code use-package} as a string.
	 *
	 * @return the function name {@code use-package} as a string
	 */
	@Override
	protected String functionName() {
		return "USE-PACKAGE";
	}
}
