/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages.functions;

import java.util.function.BiConsumer;

import jcl.packages.PackageStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code unuse-package}.
 */
@Component
public final class UnusePackageFunction extends AbstractPackageListPackageFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public UnusePackageFunction() {
		super("Causes package to cease inheriting all the external symbols of packages-to-unuse.",
		      "UNUSE-PACKAGE"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link PackageStruct#unUsePackage(PackageStruct...)} as a method reference function.
	 *
	 * @return returns {@link PackageStruct#unUsePackage(PackageStruct...)} as a method reference function
	 */
	@Override
	protected BiConsumer<PackageStruct, PackageStruct[]> packageListFunction() {
		return PackageStruct::unUsePackage;
	}

	/**
	 * {@inheritDoc}
	 * Performs default (aka. no) validation on the provided {@link PackageStruct}s.
	 *
	 * @param packageStructs
	 * 		the {@link PackageStruct}s to validate
	 */
	@Override
	protected void validatePackages(final PackageStruct... packageStructs) {
	}
}
